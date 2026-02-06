package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GShared}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** F16 Attention Output - EXACTLY matches llama.cpp's mul_mat_vec pattern.
  *
  * Computes: output[batch, qHead, dim] = Σ_k attnWeights[batch, qHead, k] * V[k, kvHead, dim]
  *
  * Dispatch pattern (EXACT llama.cpp match):
  *   - X = numOutputsPerKVHead / NUM_ROWS = (gqaRatio * headSize) / NUM_ROWS
  *   - Y = 1 (for single batch)
  *   - Z = numKVHeads
  *
  * Indexing: first_row = NUM_ROWS * (gl_WorkGroupID.x + gl_NumWorkGroups.x * gl_WorkGroupID.z)
  *
  * Each workgroup:
  *   - Processes NUM_ROWS output elements (2 outputs per workgroup)
  *   - 128 threads reduce over the K dimension (seqLen)
  *   - Uses subgroup + shared memory reduction
  */
object F16MatmulStridedVecProgram:
  val WARP_SIZE = 32
  val NUM_WARPS = 4
  val NUM_ROWS = 2    // Process 2 output elements per workgroup (matches llama.cpp)
  val BLOCK_SIZE = WARP_SIZE * NUM_WARPS  // 128 threads

  case class Sizes(
    B: Int,               // Batch size
    T: Int,               // Sequence positions (1 for decode)
    gqaRatio: Int,        // Q heads per KV head (NH / NKV)
    NKV: Int,             // Number of KV heads
    headSize: Int,        // Dimension per head
    maxSeqLen: Int,       // Max sequence length
    vCacheLayerOffset: Int, // Offset into V cache for this layer
    L: Int,               // Number of layers (for buffer sizing)
  ):
    def NH: Int = gqaRatio * NKV
    def kvSizePerPos: Int = NKV * headSize
    def fullVCacheSize: Int = L * maxSeqLen * kvSizePerPos
    def numKIterations: Int = (maxSeqLen + BLOCK_SIZE - 1) / BLOCK_SIZE
    // Outputs per KV head = T * gqaRatio * headSize
    def outputsPerKVHead: Int = T * gqaRatio * headSize
    // Workgroups in X dimension = outputsPerKVHead / NUM_ROWS
    def dispatchX: Int = (outputsPerKVHead + NUM_ROWS - 1) / NUM_ROWS
    // Y = batch (or 1 for batched attention where batch is in B*T)
    def dispatchY: Int = B
    // Z = numKVHeads (each KV head is a separate "slab")
    def dispatchZ: Int = NKV

  case class ProgramLayout(
    attnWeights: GBuffer[Float32],  // [B, T, NH, maxSeqLen]
    vCache: GBuffer[Float16],       // [L, maxSeqLen, NKV, headSize]
    output: GBuffer[Float16],       // [B, T, NH, headSize]
    params: GUniform[AttentionParams],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    // Compile-time constants
    val B = sizes.B
    val T = sizes.T
    val gqaRatio = sizes.gqaRatio
    val NKV = sizes.NKV
    val NH = sizes.NH
    val headSize = sizes.headSize
    val maxSeqLen = sizes.maxSeqLen
    val vCacheLayerOffset = sizes.vCacheLayerOffset
    val kvSizePerPos = sizes.kvSizePerPos
    val numKIterations = sizes.numKIterations
    val outputsPerKVHead = sizes.outputsPerKVHead
    val numWorkgroupsX = sizes.dispatchX  // For the llama.cpp indexing formula

    // Shared memory for partial sums from each warp
    val sharedSums = GShared[Vec2[Float32]](NUM_WARPS)

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        attnWeights = GBuffer[Float32](s.B * s.T * s.NH * s.maxSeqLen),
        vCache = GBuffer[Float16](s.fullVCacheSize),
        output = GBuffer[Float16](s.B * s.T * s.NH * s.headSize),
        params = GUniform[AttentionParams](),
      ),
      // Dispatch: [outputsPerKVHead/NUM_ROWS, batch, numKVHeads]
      dispatch = (_, s) => StaticDispatch((s.dispatchX, s.dispatchY, s.dispatchZ)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val laneId: Int32 = tid.mod(WARP_SIZE)
      val warpId: Int32 = tid / WARP_SIZE

      // Workgroup IDs
      val wgX: Int32 = GIO.workgroupId.x
      val batchIdx: Int32 = GIO.workgroupId.y
      val kvHeadIdx: Int32 = GIO.workgroupId.z  // KV head - FREE, no division!

      val runtimeParams = layout.params.read
      val seqLen: Int32 = runtimeParams.seqLen

      for
        // LLAMA.CPP EXACT INDEXING:
        // first_row = NUM_ROWS * (gl_WorkGroupID.x + gl_NumWorkGroups.x * gl_WorkGroupID.z)
        // But we separate by KV head in Z, so:
        // first_output_in_kv_head = NUM_ROWS * wgX
        firstOutputInKVHead <- GIO.pure(wgX * NUM_ROWS)
        
        // Decompose output index into (qHeadWithinGroup, dim)
        // output_idx = qHeadWithinGroup * headSize + dim
        // For row 0:
        qHeadOffset0 <- GIO.pure(firstOutputInKVHead / headSize)
        dim0 <- GIO.pure(firstOutputInKVHead.mod(headSize))
        // For row 1:
        qHeadOffset1 <- GIO.pure((firstOutputInKVHead + 1) / headSize)
        dim1 <- GIO.pure((firstOutputInKVHead + 1).mod(headSize))
        
        // Global Q head indices
        qHead0 <- GIO.pure(kvHeadIdx * gqaRatio + qHeadOffset0)
        qHead1 <- GIO.pure(kvHeadIdx * gqaRatio + qHeadOffset1)
        
        // Attention weights base: [B, T, NH, maxSeqLen]
        weightsBase0 <- GIO.pure(batchIdx * (T * NH * maxSeqLen) + qHead0 * maxSeqLen)
        weightsBase1 <- GIO.pure(batchIdx * (T * NH * maxSeqLen) + qHead1 * maxSeqLen)
        
        // Output indices: [B, T, NH, headSize]
        outIdx0 <- GIO.pure(batchIdx * (T * NH * headSize) + qHead0 * headSize + dim0)
        outIdx1 <- GIO.pure(batchIdx * (T * NH * headSize) + qHead1 * headSize + dim1)
        
        // V cache base for this layer + KV head
        // V[k, kvHead, dim] = vCache[layerOffset + k * kvSizePerPos + kvHead * headSize + dim]
        vBase0 <- GIO.pure((vCacheLayerOffset: Int32) + kvHeadIdx * headSize + dim0)
        vBase1 <- GIO.pure((vCacheLayerOffset: Int32) + kvHeadIdx * headSize + dim1)

        // Hot loop - iterate over K positions (seqLen)
        localSums <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numKIterations).fold(vec2(0.0f, 0.0f), (acc: Vec2[Float32], kPos: Int32) =>
            // Read attention weights for both rows
            val w0: Float32 = when(kPos < seqLen)(GIO.read[Float32](layout.attnWeights, weightsBase0 + kPos)).otherwise(0.0f)
            val w1: Float32 = when(kPos < seqLen)(GIO.read[Float32](layout.attnWeights, weightsBase1 + kPos)).otherwise(0.0f)
            
            // Read V values
            val vIdx0: Int32 = vBase0 + kPos * kvSizePerPos
            val vIdx1: Int32 = vBase1 + kPos * kvSizePerPos
            val v0: Float32 = GIO.read[Float16](layout.vCache, vIdx0).asFloat32
            val v1: Float32 = GIO.read[Float16](layout.vCache, vIdx1).asFloat32
            
            vec2(acc.x + w0 * v0, acc.y + w1 * v1)
          )
        }

        // Subgroup reduction within each warp
        warpSum <- GIO.pure(vec2(GIO.subgroupAdd(localSums.x), GIO.subgroupAdd(localSums.y)))
        
        // First thread of each warp writes to shared memory
        _ <- GIO.when(laneId === 0)(sharedSums.write(warpId, warpSum))
        _ <- GIO.barrier

        // Reduce across warps
        s0 <- GIO.pure(sharedSums.read(0))
        s1 <- GIO.pure(sharedSums.read(1))
        s2 <- GIO.pure(sharedSums.read(2))
        s3 <- GIO.pure(sharedSums.read(3))
        total0 <- GIO.pure(s0.x + s1.x + s2.x + s3.x)
        total1 <- GIO.pure(s0.y + s1.y + s2.y + s3.y)
        
        // Thread 0 writes both output values
        // Check bounds for row 1 (row 0 always valid if workgroup launched)
        _ <- GIO.when(tid === 0):
          for
            _ <- GIO.write[Float16](layout.output, outIdx0, total0.asFloat16)
            _ <- GIO.when(firstOutputInKVHead + 1 < outputsPerKVHead):
              GIO.write[Float16](layout.output, outIdx1, total1.asFloat16)
          yield GStruct.Empty()
      yield GStruct.Empty()
