package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct.Empty

/** RMSNorm (Root Mean Square Normalization) using subgroup reductions.
  * 
  * RMSNorm is simpler than LayerNorm - it doesn't subtract mean:
  *   output = x * rsqrt(mean(x^2) + eps) * weight
  * 
  * Uses hardware subgroup operations for efficient parallel reduction.
  * One subgroup (32 threads) processes one row in strided fashion.
  * 
  * Supports layered weights via weightOffset (defaults to 0 for standalone use).
  */
object RMSNormProgram:

  val WARP_SIZE = 32

  case class Sizes(
    numRows: Int,
    rowSize: Int,
    eps: Float,
    weightOffset: Int = 0,
    totalWeightSize: Int = -1,  // -1 means use rowSize (single layer)
  ):
    def numIterations: Int = (rowSize + WARP_SIZE - 1) / WARP_SIZE
    def actualWeightSize: Int = if totalWeightSize < 0 then rowSize else totalWeightSize

  case class ProgramLayout(
    input: GBuffer[Float32],
    weight: GBuffer[Float32],
    output: GBuffer[Float32],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val rowSize = sizes.rowSize
    val eps = sizes.eps
    val numIterations = sizes.numIterations
    val weightOffset = sizes.weightOffset

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        input = GBuffer[Float32](s.numRows * s.rowSize),
        weight = GBuffer[Float32](s.actualWeightSize),
        output = GBuffer[Float32](s.numRows * s.rowSize),
      ),
      dispatch = (_, s) =>
        val warpsPerWorkgroup = 8
        val numWorkgroups = (s.numRows + warpsPerWorkgroup - 1) / warpsPerWorkgroup
        StaticDispatch((numWorkgroups, 1, 1)),
      workgroupSize = (256, 1, 1),
    ): layout =>
      val globalId = GIO.invocationId
      val laneId = globalId.mod(WARP_SIZE)
      val subgroupIdx = globalId / WARP_SIZE
      val rowSizeVal: Int32 = rowSize
      val epsVal: Float32 = eps
      val weightOffsetVal: Int32 = weightOffset
      val numRowsVal: Int32 = sizes.numRows

      GIO.when(subgroupIdx < numRowsVal):
        val rowIdx = subgroupIdx
        val baseIdx = rowIdx * rowSizeVal

        val localSumSq = GSeq
          .gen[Int32](laneId, _ + WARP_SIZE)
          .limit(numIterations)
          .fold(0.0f, (sum: Float32, i: Int32) =>
            when(i < rowSizeVal):
              val x = GIO.read[Float32](layout.input, baseIdx + i)
              sum + x * x
            .otherwise(sum)
          )

        val totalSumSq = GIO.subgroupAdd(localSumSq)
        val meanSq = totalSumSq / rowSizeVal.asFloat
        val scale: Float32 = 1.0f / sqrt(meanSq + epsVal)

        GIO.repeat(numIterations): j =>
          val i = laneId + j * WARP_SIZE
          GIO.when(i < rowSizeVal):
            val x = GIO.read[Float32](layout.input, baseIdx + i)
            val w = GIO.read[Float32](layout.weight, weightOffsetVal + i)
            GIO.write[Float32](layout.output, baseIdx + i, x * scale * w)
