package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GBuffer
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Attention output: attn_weights · V
  *
  * OPTIMIZED with 3D dispatch to eliminate integer divisions:
  *   - X = dimQuadIdx (16 workgroups for headSize=64)
  *   - Y = headIdx (32 Q heads)
  *   - Z = batch * T (1 for decode)
  *
  *   - 32 threads (1 warp) - no cross-warp reduction needed
  *   - Subgroup reduction only (fast path)
  *
  * V cache is TRANSPOSED: [layer][head][dim][pos]
  * This ensures consecutive threads (different positions) read consecutive memory,
  * achieving 100% memory coalescing vs 0.8% with the old layout.
  */
object F16AttentionOutputProgram:
  val WARP_SIZE = 32
  val NUM_DIMS = 4   // Process 4 output dimensions per workgroup (Vec4)
  val BLOCK_SIZE = WARP_SIZE  // 32 threads - single warp

  case class Sizes(
    B: Int,
    T: Int,
    NH: Int,
    NKV: Int,
    headSize: Int,
    maxSeqLen: Int,
    vCacheLayerOffset: Int,
    L: Int,
  ):
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
    def dispatchX: Int = (headSize + NUM_DIMS - 1) / NUM_DIMS  // dimQuad workgroups (16)
    def dispatchY: Int = NH  // Q heads (32)
    def dispatchZ: Int = B * T  // batch × positions (1 for decode)

  case class ProgramLayout(
    attnWeights: GBuffer[Float32],
    vCache: GBuffer[Float16],
    output: GBuffer[Float16],
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
    val dimStride = sizes.dimStride      // = maxSeqLen
    val headStride = sizes.headStride    // = headSize * maxSeqLen
    val numKIterations = sizes.numKIterations

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        attnWeights = GBuffer[Float32](s.B * s.T * s.NH * s.maxSeqLen),
        vCache = GBuffer[Float16](s.fullCacheSize),
        output = GBuffer[Float16](s.B * s.T * s.NH * s.headSize),
        params = GUniform[AttentionParams](),
      ),
      // 3D dispatch: [dimQuads, heads, batch*T]
      dispatch = (_, s) => StaticDispatch((s.dispatchX, s.dispatchY, s.dispatchZ)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x

      // 3D workgroup IDs - NO DIVISIONS FOR DISPATCH!
      val dimQuadIdx: Int32 = GIO.workgroupId.x
      val headIdx: Int32 = GIO.workgroupId.y
      val batchPosIdx: Int32 = GIO.workgroupId.z

      val runtimeParams = layout.params.read
      val seqLen: Int32 = runtimeParams.seqLen

      for
        // KV head from Q head - only ONE division (gqaRatio is compile-time)
        kvHeadIdx <- GIO.pure(headIdx / gqaRatio)
        
        // Output dimensions for this workgroup (4 dims)
        outDim0 <- GIO.pure(dimQuadIdx * NUM_DIMS)
        
        // Flat head index for weights/output addressing
        flatHeadIdx <- GIO.pure(batchPosIdx * NH + headIdx)
        
        // Pre-compute base addresses ONCE
        weightsBase <- GIO.pure(flatHeadIdx * maxSeqLen)
        outBase <- GIO.pure(flatHeadIdx * headSize + outDim0)
        
        // V cache base for this KV head (TRANSPOSED layout: [layer][head][dim][pos])
        // Base address: layerOffset + kvHead * headStride
        // For dim d: + d * dimStride
        // For pos p: + p (consecutive!)
        vCacheHeadBase <- GIO.pure((vCacheLayerOffset: Int32) + kvHeadIdx * headStride)
        
        // Pre-compute base for each of the 4 dims (each dim's positions are consecutive)
        vBase0 <- GIO.pure(vCacheHeadBase + outDim0 * dimStride)
        vBase1 <- GIO.pure(vCacheHeadBase + (outDim0 + 1) * dimStride)
        vBase2 <- GIO.pure(vCacheHeadBase + (outDim0 + 2) * dimStride)
        vBase3 <- GIO.pure(vCacheHeadBase + (outDim0 + 3) * dimStride)

        // Hot loop - 32 threads cooperate
        // Each thread reads positions tid, tid+32, tid+64, ...
        // Consecutive threads read consecutive positions → COALESCED!
        localSums <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numKIterations).unroll.fold(vec4(0.0f, 0.0f, 0.0f, 0.0f), (acc: Vec4[Float32], kPos: Int32) =>
            // Read weight (softmax output is 0 for invalid positions)
            val weight: Float32 = when(kPos < seqLen)(GIO.read[Float32](layout.attnWeights, weightsBase + kPos)).otherwise(0.0f)
            
            // Read 4 V values from TRANSPOSED cache (4 scalar reads, each coalesced across threads)
            // Thread 0 reads pos 0, Thread 1 reads pos 1, ... → consecutive memory!
            val v0: Float32 = GIO.read[Float16](layout.vCache, vBase0 + kPos).asFloat32
            val v1: Float32 = GIO.read[Float16](layout.vCache, vBase1 + kPos).asFloat32
            val v2: Float32 = GIO.read[Float16](layout.vCache, vBase2 + kPos).asFloat32
            val v3: Float32 = GIO.read[Float16](layout.vCache, vBase3 + kPos).asFloat32
            
            vec4(
              acc.x + weight * v0,
              acc.y + weight * v1,
              acc.z + weight * v2,
              acc.w + weight * v3,
            )
          )
        }

        // Subgroup reduction - single warp means subgroupAdd gives final result
        total <- GIO.pure(vec4(
          GIO.subgroupAdd(localSums.x),
          GIO.subgroupAdd(localSums.y),
          GIO.subgroupAdd(localSums.z),
          GIO.subgroupAdd(localSums.w),
        ))
        
        // Thread 0 writes all 4 output values
        _ <- GIO.when(tid === 0):
          for
            _ <- GIO.write[Float16](layout.output, outBase, total.x.asFloat16)
            _ <- GIO.write[Float16](layout.output, outBase + 1, total.y.asFloat16)
            _ <- GIO.write[Float16](layout.output, outBase + 2, total.z.asFloat16)
            _ <- GIO.write[Float16](layout.output, outBase + 3, total.w.asFloat16)
          yield GStruct.Empty()
      yield GStruct.Empty()
