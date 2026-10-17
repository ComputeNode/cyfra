package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.core.memory.{GBuffer, SharedVariable}
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Attention output: attn_weights · V
  *
  * OPTIMIZED with 3D dispatch to eliminate integer divisions:
  *   - X = dimQuadIdx (16 workgroups for headSize=64)
  *   - Y = headIdx (32 Q heads)
  *   - Z = batch * T (1 for decode)
  *   - 32 threads (1 warp) - no cross-warp reduction needed
  *   - Subgroup reduction only (fast path)
  *
  * V cache is TRANSPOSED: [layer][head][dim][pos] This ensures consecutive threads (different positions) read consecutive memory, achieving 100%
  * memory coalescing vs 0.8% with the old layout.
  */
object F16AttentionOutputProgram:
  val WARP_SIZE = 32
  val NUM_WARPS = 4 // Use 4 warps (128 threads) for more parallelism
  val NUM_DIMS = 4 // Process 4 output dimensions per workgroup (Vec4)
  val BLOCK_SIZE = WARP_SIZE * NUM_WARPS // 128 threads

  case class Sizes(B: Int, T: Int, NH: Int, NKV: Int, headSize: Int, maxSeqLen: Int, vCacheLayerOffset: Int, L: Int):
    def gqaRatio: Int = NH / NKV
    def kvSizePerPos: Int = NKV * headSize
    def fullCacheSize: Int = L * maxSeqLen * kvSizePerPos
    // Size of one dim's position array in transposed V cache
    def dimStride: Int = maxSeqLen
    // Size of one head's data in transposed V cache
    def headStride: Int = headSize * maxSeqLen
    // Number of iterations over K positions per thread
    def numKIterations: Int = (maxSeqLen + BLOCK_SIZE - 1) / BLOCK_SIZE
    // 3D dispatch dimensions
    def dispatchX: Int = (headSize + NUM_DIMS - 1) / NUM_DIMS // dimQuad workgroups (16)
    def dispatchY: Int = NH // Q heads (32)
    def dispatchZ: Int = B * T // batch × positions (1 for decode)

  case class ProgramLayout(
    attnWeights: GBuffer[RuntimeArray[Float32]],
    vCache: GBuffer[RuntimeArray[Float16]],
    output: GBuffer[RuntimeArray[Float16]],
    params: GUniform[AttentionParams],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val B = sizes.B
    val T = sizes.T
    val NH = sizes.NH
    val NKV = sizes.NKV
    val headSize = sizes.headSize
    val gqaRatio = sizes.gqaRatio
    val vCacheLayerOffset = sizes.vCacheLayerOffset
    val maxSeqLen = sizes.maxSeqLen
    val dimStride = sizes.dimStride // = maxSeqLen
    val headStride = sizes.headStride // = headSize * maxSeqLen
    val numKIterations = sizes.numKIterations

    GioProgram[Sizes, ProgramLayout](
      layout = s =>
        ProgramLayout(
          attnWeights = GBuffer[RuntimeArray[Float32]](s.B * s.T * s.NH * s.maxSeqLen),
          vCache = GBuffer[RuntimeArray[Float16]](s.fullCacheSize),
          output = GBuffer[RuntimeArray[Float16]](s.B * s.T * s.NH * s.headSize),
          params = GUniform[AttentionParams](),
        ),
      // 3D dispatch: [dimQuads, heads, batch*T]
      dispatch = (_, s) => StaticDispatch((s.dispatchX, s.dispatchY, s.dispatchZ)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val warpId: Int32 = tid / WARP_SIZE
      val laneId: Int32 = tid % WARP_SIZE

      // 3D workgroup IDs - NO DIVISIONS FOR DISPATCH!
      val dimQuadIdx: Int32 = GIO.workgroupId.x
      val headIdx: Int32 = GIO.workgroupId.y
      val batchPosIdx: Int32 = GIO.workgroupId.z

      val runtimeParams = GIO.read(layout.params)
      val seqLen: Int32 = runtimeParams.seqLen

      // Shared memory for cross-warp reduction (4 warps × 4 dims)
      val sharedSums = SharedVariable[SizedArray[Vec4[Float32], NUM_WARPS.type]]()

      // KV head from Q head - only ONE division (gqaRatio is compile-time)
      val kvHeadIdx = GIO.eval(headIdx / gqaRatio)

      // Output dimensions for this workgroup (4 dims)
      val outDim0 = GIO.eval(dimQuadIdx * NUM_DIMS)

      // Flat head index for weights/output addressing
      val flatHeadIdx = GIO.eval(batchPosIdx * NH + headIdx)

      // Pre-compute base addresses ONCE
      val weightsBase = GIO.eval(flatHeadIdx * maxSeqLen)
      val outBase = GIO.eval(flatHeadIdx * headSize + outDim0)

      // V cache base for this KV head (TRANSPOSED layout: [layer][head][dim][pos])
      // Base address: layerOffset + kvHead * headStride
      // For dim d: + d * dimStride
      // For pos p: + p (consecutive!)
      val vCacheHeadBase = GIO.eval((vCacheLayerOffset: Int32) + kvHeadIdx * headStride)

      // Pre-compute base for each of the 4 dims (each dim's positions are consecutive)
      val vBase0 = GIO.eval(vCacheHeadBase + outDim0 * dimStride)
      val vBase1 = GIO.eval(vCacheHeadBase + (outDim0 + 1) * dimStride)
      val vBase2 = GIO.eval(vCacheHeadBase + (outDim0 + 2) * dimStride)
      val vBase3 = GIO.eval(vCacheHeadBase + (outDim0 + 3) * dimStride)

      // Hot loop - 128 threads cooperate (4 warps)
      // Each thread reads positions tid, tid+128, tid+256, ...
      // Consecutive threads read consecutive positions → COALESCED!
      val localSums = GIO.foldRepeat[Vec4[Float32]](numKIterations, Vec4(0.0f, 0.0f, 0.0f, 0.0f), unroll = true) { (iter, acc) =>
        val kPos: Int32 = tid + iter * BLOCK_SIZE
        // Read weight (softmax output is 0 for invalid positions)
        val weight: Float32 = GIO.branch(kPos < seqLen, GIO.read(layout.attnWeights.focus(_.at(weightsBase + kPos))), Float32(0.0f))

        // Read 4 V values from TRANSPOSED cache (4 scalar reads, each coalesced across threads)
        // Thread 0 reads pos 0, Thread 1 reads pos 1, ... → consecutive memory!
        val v0: Float32 = GIO.read(layout.vCache.focus(_.at(vBase0 + kPos))).asFloat32
        val v1: Float32 = GIO.read(layout.vCache.focus(_.at(vBase1 + kPos))).asFloat32
        val v2: Float32 = GIO.read(layout.vCache.focus(_.at(vBase2 + kPos))).asFloat32
        val v3: Float32 = GIO.read(layout.vCache.focus(_.at(vBase3 + kPos))).asFloat32

        Vec4(acc.x + weight * v0, acc.y + weight * v1, acc.z + weight * v2, acc.w + weight * v3)
      }

      // First: subgroup reduction within each warp
      val warpSum =
        GIO.eval(Vec4(GIO.subgroupAdd(localSums.x), GIO.subgroupAdd(localSums.y), GIO.subgroupAdd(localSums.z), GIO.subgroupAdd(localSums.w)))

      // Lane 0 of each warp writes to shared memory
      GIO.when(laneId === 0):
        GIO.write(sharedSums.focus(_.at(warpId)), warpSum)

      GIO.barrier

      // Thread 0 reads all warp sums and writes final result
      GIO.when(tid === 0):
        val sum0 = GIO.read(sharedSums.focus(_.at(0)))
        val sum1 = GIO.read(sharedSums.focus(_.at(1)))
        val sum2 = GIO.read(sharedSums.focus(_.at(2)))
        val sum3 = GIO.read(sharedSums.focus(_.at(3)))
        val total = GIO.eval(
          Vec4(
            sum0.x + sum1.x + sum2.x + sum3.x,
            sum0.y + sum1.y + sum2.y + sum3.y,
            sum0.z + sum1.z + sum2.z + sum3.z,
            sum0.w + sum1.w + sum2.w + sum3.w,
          ),
        )
        GIO.write(layout.output.focus(_.at(outBase)), total.x.asFloat16)
        GIO.write(layout.output.focus(_.at(outBase + 1)), total.y.asFloat16)
        GIO.write(layout.output.focus(_.at(outBase + 2)), total.z.asFloat16)
        GIO.write(layout.output.focus(_.at(outBase + 3)), total.w.asFloat16)
