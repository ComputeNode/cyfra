package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.{*, given}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GShared
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.llama.programs.AttentionParams

/** Fused Attention: Q·K^T → softmax → ·V in one kernel.
  *
  * Merges F16AttentionScoresProgram + F16AttentionSoftmaxProgram + F16AttentionOutputProgram.
  * Scores are kept in shared memory instead of global memory.
  *
  * OPTIMIZED Phase 3:
  *   - Parallel reduction across 32 threads with subgroup operations
  *   - Each iteration processes one output dimension, all threads cooperate
  *   - Coalesced V cache reads (transposed layout)
  *
  * Dispatch: One workgroup per (batch, queryPos, head).
  * BLOCK_SIZE = 32 (single warp) for efficient subgroup operations.
  */
object F16FusedAttentionProgram:
  val BLOCK_SIZE = 32

  case class Sizes(
    B: Int,
    T: Int,
    NH: Int,
    NKV: Int,
    headSize: Int,
    maxSeqLen: Int,
    kCacheLayerOffset: Int,
    vCacheLayerOffset: Int,
    L: Int,
  ):
    def gqaRatio: Int = NH / NKV
    def kvSizePerPos: Int = NKV * headSize
    def fullCacheSize: Int = L * maxSeqLen * kvSizePerPos
    def numIterations: Int = (maxSeqLen + BLOCK_SIZE - 1) / BLOCK_SIZE
    def numWorkgroups: Int = B * T * NH
    // V cache strides (transposed layout: [layer][head][dim][pos])
    def dimStride: Int = maxSeqLen
    def headStride: Int = headSize * maxSeqLen

  case class ProgramLayout(
    q: GBuffer[Float16],
    kCache: GBuffer[Float16],
    vCache: GBuffer[Float16],
    output: GBuffer[Float16],
    params: GUniform[AttentionParams],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    given Sizes = sizes
    val B = sizes.B
    val T = sizes.T
    val NH = sizes.NH
    val headSize = sizes.headSize
    val gqaRatio = sizes.gqaRatio
    val maxSeqLen = sizes.maxSeqLen
    val kCacheLayerOffset = sizes.kCacheLayerOffset
    val vCacheLayerOffset = sizes.vCacheLayerOffset
    val kvSizePerPos = sizes.kvSizePerPos
    val numIterations = sizes.numIterations
    val dimStride = sizes.dimStride
    val headStride = sizes.headStride
    val scale = 1.0f / math.sqrt(headSize).toFloat

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        q = GBuffer.sized[Float16](s.B * s.T * s.NH * s.headSize),
        kCache = GBuffer.sized[Float16](s.fullCacheSize),
        vCache = GBuffer.sized[Float16](s.fullCacheSize),
        output = GBuffer.sized[Float16](s.B * s.T * s.NH * s.headSize),
        params = GUniform[AttentionParams](),
      ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x

      val runtimeParams = layout.params.read
      val seqLen: Int32 = runtimeParams.seqLen
      val startPos: Int32 = runtimeParams.startPos

      // Shared memory for scores
      val sharedScores = GShared[Float32](maxSeqLen)

      for
        // ========== COMPUTE INDICES ==========
        batchIdx <- GIO.pure(workgroupId / (T * NH))
        remainder <- GIO.pure(workgroupId.mod(T * NH))
        queryPosLocal <- GIO.pure(remainder / NH)
        headIdx <- GIO.pure(remainder.mod(NH))
        kvHeadIdx <- GIO.pure(headIdx / gqaRatio)
        queryPosGlobal <- GIO.pure(startPos + queryPosLocal)

        qBase <- GIO.pure(batchIdx * (T * NH * headSize) + queryPosLocal * (NH * headSize) + headIdx * headSize)
        kCacheBase0 <- GIO.pure((kCacheLayerOffset: Int32) + kvHeadIdx * headSize)
        outBase <- GIO.pure(batchIdx * (T * NH * headSize) + queryPosLocal * (NH * headSize) + headIdx * headSize)
        vCacheHeadBase <- GIO.pure((vCacheLayerOffset: Int32) + kvHeadIdx * headStride)

        // ========== PHASE 1: SCORES (Q·K^T) ==========
        _ <- GIO.foldRepeat[GStruct.Empty](numIterations, GStruct.Empty()): (iter, _) =>
          val kPos: Int32 = tid + iter * BLOCK_SIZE
          val kCacheBase: Int32 = kCacheBase0 + kPos * kvSizePerPos

          val dot: Float32 = GSeq.gen[Int32](0, _ + 1).limit(headSize).unroll.fold(0.0f, (acc: Float32, d: Int32) =>
            val qVal: Float32 = GIO.read[Float16](layout.q, qBase + d).asFloat32
            val kVal: Float32 = GIO.read[Float16](layout.kCache, kCacheBase + d).asFloat32
            acc + qVal * kVal
          )

          val isValid = kPos <= queryPosGlobal && kPos < seqLen
          val score: Float32 = when(isValid)(dot * scale).otherwise(-10000.0f)
          sharedScores.write(kPos, score)
        _ <- GIO.barrier

        // ========== PHASE 2: SOFTMAX ==========
        // Find max
        localMax <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numIterations).fold(-10000.0f, (maxVal: Float32, col: Int32) =>
            val isValid = col <= queryPosGlobal && col < seqLen
            val score: Float32 = sharedScores.read(col)
            when(isValid)(max(maxVal, score)).otherwise(maxVal)
          )
        }
        globalMax <- GIO.pure(GIO.subgroupMax(localMax))

        // Compute exp sum
        localSum <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numIterations).fold(0.0f, (sumVal: Float32, col: Int32) =>
            val isValid = col <= queryPosGlobal && col < seqLen
            val score: Float32 = sharedScores.read(col)
            val expScore: Float32 = exp(score - globalMax)
            when(isValid)(sumVal + expScore).otherwise(sumVal)
          )
        }
        globalSum <- GIO.pure(GIO.subgroupAdd(localSum) + 0.0000001f)
        rcpSum <- GIO.pure(1.0f / globalSum)

        // Normalize in-place
        _ <- GIO.foldRepeat[GStruct.Empty](numIterations, GStruct.Empty()): (iter, _) =>
          val col: Int32 = tid + iter * BLOCK_SIZE
          val isValid = col <= queryPosGlobal && col < seqLen
          val score: Float32 = sharedScores.read(col)
          val expScore: Float32 = exp(score - globalMax)
          val result: Float32 = when(isValid)(expScore * rcpSum).otherwise(0.0f)
          sharedScores.write(col, result)
        _ <- GIO.barrier

        // ========== PHASE 3: OUTPUT (weights · V) - PARALLEL REDUCTION ==========
        // Each iteration: all 32 threads cooperate to compute one output dimension
        // Thread tid handles positions tid, tid+32, tid+64, ... then subgroupAdd
        _ <- GIO.foldRepeat[GStruct.Empty](headSize, GStruct.Empty()): (outDim, _) =>
          val vBase: Int32 = vCacheHeadBase + outDim * dimStride

          // Each thread computes partial sum for positions tid, tid+32, tid+64, ...
          val localSum: Float32 = GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numIterations).unroll.fold(0.0f, (acc: Float32, kPos: Int32) =>
            val weight: Float32 = when(kPos < seqLen)(sharedScores.read(kPos)).otherwise(0.0f)
            val vVal: Float32 = GIO.read[Float16](layout.vCache, vBase + kPos).asFloat32
            acc + weight * vVal
          )

          // Subgroup reduction - all 32 threads contribute
          val total: Float32 = GIO.subgroupAdd(localSum)

          // Thread 0 writes the result
          GIO.when(tid === 0):
            GIO.write[Float16](layout.output, outBase + outDim, total.asFloat16)
      yield GStruct.Empty()
