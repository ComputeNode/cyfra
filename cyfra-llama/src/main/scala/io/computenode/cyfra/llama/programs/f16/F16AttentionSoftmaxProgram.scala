package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.llama.programs.AttentionParams

/** Attention softmax: softmax(scores) in-place
  *
  * Applies softmax to attention scores with causal masking. One workgroup per row (batch, queryPos, head). Uses subgroup operations for reduction.
  *
  * IMPORTANT: Writes 0.0 to invalid positions so output kernel can skip bounds checking.
  */
object F16AttentionSoftmaxProgram:
  val BLOCK_SIZE = 32

  case class Sizes(B: Int, T: Int, NH: Int, maxSeqLen: Int):
    def numRows: Int = B * T * NH
    def numIterations: Int = (maxSeqLen + BLOCK_SIZE - 1) / BLOCK_SIZE

  case class ProgramLayout(
    scores: GBuffer[GArray[Float32]], // in-place: [B, T, NH, maxSeqLen]
    params: GUniform[AttentionParams],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val B = sizes.B
    val T = sizes.T
    val NH = sizes.NH
    val maxSeqLen = sizes.maxSeqLen
    val numIterations = sizes.numIterations

    GioProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(scores = GBuffer[GArray[Float32]](s.B * s.T * s.NH * s.maxSeqLen), params = GUniform[AttentionParams]()),
      dispatch = (_, s) => StaticDispatch((s.numRows, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val rowIdx: Int32 = GIO.workgroupId.x

      val runtimeParams = GIO.read(layout.params)
      val seqLen: Int32 = runtimeParams.seqLen
      val startPos: Int32 = runtimeParams.startPos

      // Derive query position from row index
      val remainder = GIO.eval(rowIdx % (T * NH))
      val queryPosLocal = GIO.eval(remainder / NH)
      val queryPosGlobal = GIO.eval(startPos + queryPosLocal)
      val rowBase = GIO.eval(rowIdx * maxSeqLen)

      // Phase 1: Find local max value (only over valid positions)
      val localMax = GIO.foldRepeat[Float32](numIterations, -10000.0f) { (iter, maxVal) =>
        val col: Int32 = tid + iter * BLOCK_SIZE
        val isValid = col <= queryPosGlobal && col < seqLen
        GIO.branch(isValid, max(maxVal, GIO.read(layout.scores.focus(_.at(rowBase + col)))), maxVal)
      }

      // Reduce max within warp using subgroupMax
      val globalMax = GIO.subgroupMax(localMax)

      // Phase 2: Compute exp(x - max) and local sum (only over valid positions)
      val localSum = GIO.foldRepeat[Float32](numIterations, 0.0f) { (iter, sumVal) =>
        val col: Int32 = tid + iter * BLOCK_SIZE
        val isValid = col <= queryPosGlobal && col < seqLen
        GIO.branch(isValid, sumVal + exp(GIO.read(layout.scores.focus(_.at(rowBase + col))) - globalMax), sumVal)
      }

      // Reduce sum within warp using subgroupAdd
      val globalSum = GIO.eval(GIO.subgroupAdd(localSum) + 0.0000001f)
      val rcpSum = GIO.eval(1.0f / globalSum)

      // Phase 3: Normalize in-place
      // Write normalized value for valid positions, 0.0 for invalid
      GIO.repeat(numIterations): iter =>
        val col: Int32 = tid + iter * BLOCK_SIZE
        val isValid = col <= queryPosGlobal && col < seqLen
        // Valid positions get normalized value, invalid get 0.0.
        val result = GIO.branch(isValid, exp(GIO.read(layout.scores.focus(_.at(rowBase + col))) - globalMax) * rcpSum, Float32(0.0f))
        GIO.write(layout.scores.focus(_.at(rowBase + col)), result)
