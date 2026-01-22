package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** F16 Root Mean Square Layer Normalization.
  *
  * Normalizes input by RMS: `output[i] = input[i] / rms(input) * weight[i]`.
  * Accumulates in F32 for numerical precision.
  */
object F16RMSNormProgram:
  val WARP_SIZE = 32
  
  case class Sizes(
    numRows: Int,
    rowSize: Int,
    eps: Float,
    weightOffset: Int = 0,
    totalWeightSize: Int = -1,
  ):
    def numIterations: Int = (rowSize + WARP_SIZE - 1) / WARP_SIZE
    def actualWeightSize: Int = if totalWeightSize < 0 then rowSize else totalWeightSize
  
  case class ProgramLayout(
    input: GBuffer[Float16],
    weight: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout
  
  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val rowSize = sizes.rowSize
    val eps = sizes.eps
    val numIterations = sizes.numIterations
    val weightOffset = sizes.weightOffset
    
    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        input = GBuffer[Float16](s.numRows * s.rowSize),
        weight = GBuffer[Float16](s.actualWeightSize),
        output = GBuffer[Float16](s.numRows * s.rowSize),
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
              val x = GIO.read[Float16](layout.input, baseIdx + i).asFloat32
              sum + (x * x)
            .otherwise(sum)
          )
        
        val totalSumSq = GIO.subgroupAdd(localSumSq)
        val rowSizeF32 = rowSizeVal.asFloat
        val meanSq: Float32 = totalSumSq / rowSizeF32
        val scale: Float32 = 1.0f / sqrt(meanSq + epsVal)
        
        GIO.repeat(numIterations): j =>
          val i = laneId + j * WARP_SIZE
          GIO.when(i < rowSizeVal):
            val x = GIO.read[Float16](layout.input, baseIdx + i).asFloat32
            val w = GIO.read[Float16](layout.weight, weightOffsetVal + i).asFloat32
            GIO.write[Float16](layout.output, baseIdx + i, (x * scale * w).asFloat16)
