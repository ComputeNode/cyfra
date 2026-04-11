package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Attention softmax: softmax(scores) in-place
  *
  * Applies softmax to attention scores with causal masking.
  * One workgroup per row (batch, queryPos, head).
  * Uses subgroup operations for reduction.
  *
  * IMPORTANT: Writes 0.0 to invalid positions so output kernel can skip bounds checking.
  */
object F16AttentionSoftmaxProgram:
  val BLOCK_SIZE = 32

  case class Sizes(
    B: Int,
    T: Int,
    NH: Int,
    maxSeqLen: Int,
  ):
    def numRows: Int = B * T * NH
    def numIterations: Int = (maxSeqLen + BLOCK_SIZE - 1) / BLOCK_SIZE

  case class ProgramLayout(
    scores: GBuffer[Float32], // in-place: [B, T, NH, maxSeqLen]
    params: GUniform[AttentionParams],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val B = sizes.B
    val T = sizes.T
    val NH = sizes.NH
    val maxSeqLen = sizes.maxSeqLen
    val numIterations = sizes.numIterations

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        scores = GBuffer[Float32](s.B * s.T * s.NH * s.maxSeqLen),
        params = GUniform[AttentionParams](),
      ),
      dispatch = (_, s) => StaticDispatch((s.numRows, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val rowIdx: Int32 = GIO.workgroupId.x

      val runtimeParams = layout.params.read
      val seqLen: Int32 = runtimeParams.seqLen
      val startPos: Int32 = runtimeParams.startPos

      for
        // Derive query position from row index
        remainder <- GIO.pure(rowIdx.mod(T * NH))
        queryPosLocal <- GIO.pure(remainder / NH)
        queryPosGlobal <- GIO.pure(startPos + queryPosLocal)
        rowBase <- GIO.pure(rowIdx * maxSeqLen)

        // Phase 1: Find local max value (only over valid positions)
        localMax <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numIterations).fold(-10000.0f, (maxVal: Float32, col: Int32) =>
            val isValid = col <= queryPosGlobal && col < seqLen
            val score: Float32 = GIO.read[Float32](layout.scores, rowBase + col)
            when(isValid)(max(maxVal, score)).otherwise(maxVal)
          )
        }

        // Reduce max within warp using subgroupMax
        globalMax <- GIO.pure(GIO.subgroupMax(localMax))

        // Phase 2: Compute exp(x - max) and local sum (only over valid positions)
        localSum <- GIO.pure {
          GSeq.gen[Int32](tid, _ + BLOCK_SIZE).limit(numIterations).fold(0.0f, (sumVal: Float32, col: Int32) =>
            val isValid = col <= queryPosGlobal && col < seqLen
            val score: Float32 = GIO.read[Float32](layout.scores, rowBase + col)
            val expScore: Float32 = exp(score - globalMax)
            when(isValid)(sumVal + expScore).otherwise(sumVal)
          )
        }

        // Reduce sum within warp using subgroupAdd
        globalSum <- GIO.pure(GIO.subgroupAdd(localSum) + 0.0000001f)
        rcpSum <- GIO.pure(1.0f / globalSum)

        // Phase 3: Normalize in-place
        // Write normalized value for valid positions, 0.0 for invalid
        _ <- GIO.foldRepeat[GStruct.Empty](numIterations, GStruct.Empty()): (iter, _) =>
          val col: Int32 = tid + iter * BLOCK_SIZE
          val isValid = col <= queryPosGlobal && col < seqLen
          val score: Float32 = GIO.read[Float32](layout.scores, rowBase + col)
          val expScore: Float32 = exp(score - globalMax)
          // Valid positions get normalized value, invalid get 0.0
          val result: Float32 = when(isValid)(expScore * rcpSum).otherwise(0.0f)
          GIO.write[Float32](layout.scores, rowBase + col, result)
      yield GStruct.Empty()
