package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GShared}
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
  *   - Vec4 reads for V cache
  *   - Subgroup reduction only (fast path)
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
    val kvSizePerPos = sizes.kvSizePerPos
    val maxSeqLen = sizes.maxSeqLen
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
        vCacheHeadBase <- GIO.pure((vCacheLayerOffset: Int32) + kvHeadIdx * headSize + outDim0)

        // Hot loop - 32 threads cooperate
        localSums <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numKIterations).unroll.fold(vec4(0.0f, 0.0f, 0.0f, 0.0f), (acc: Vec4[Float32], kPos: Int32) =>
            // Read weight (softmax output is 0 for invalid positions)
            val weight: Float32 = when(kPos < seqLen)(GIO.read[Float32](layout.attnWeights, weightsBase + kPos)).otherwise(0.0f)
            
            // Read 4 V values
            val vIdx: Int32 = vCacheHeadBase + kPos * kvSizePerPos
            val vVec: Vec4[Float16] = layout.vCache.readVec4(vIdx)
            
            vec4(
              acc.x + weight * vVec.x.asFloat32,
              acc.y + weight * vVec.y.asFloat32,
              acc.z + weight * vVec.z.asFloat32,
              acc.w + weight * vVec.w.asFloat32,
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
