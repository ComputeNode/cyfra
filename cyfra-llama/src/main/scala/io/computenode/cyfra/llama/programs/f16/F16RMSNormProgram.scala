package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.core.memory.GShared

/** F16 Root Mean Square Layer Normalization.
  *
  * Normalizes input by RMS: `output[i] = input[i] / rms(input) * weight[i]`. Accumulates in F32 for numerical precision.
  */
object F16RMSNormProgram:
  val BLOCK_SIZE = 512

  case class Sizes(numRows: Int, rowSize: Int, eps: Float, weightOffset: Int = 0, totalWeightSize: Int = -1):
    def numIterations: Int = (rowSize + BLOCK_SIZE - 1) / BLOCK_SIZE
    def actualWeightSize: Int = if totalWeightSize < 0 then rowSize else totalWeightSize

  case class ProgramLayout(input: GBuffer[GArray[Float16]], weight: GBuffer[GArray[Float16]], output: GBuffer[GArray[Float16]]) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val rowSize = sizes.rowSize
    val eps = sizes.eps
    val numIterations = sizes.numIterations
    val weightOffset = sizes.weightOffset

    GioProgram[Sizes, ProgramLayout](
      layout = s =>
        ProgramLayout(
          input = GBuffer[GArray[Float16]](s.numRows * s.rowSize),
          weight = GBuffer[GArray[Float16]](s.actualWeightSize),
          output = GBuffer[GArray[Float16]](s.numRows * s.rowSize),
        ),
      dispatch = (_, s) => StaticDispatch((s.numRows, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid = GIO.localInvocationIndex
      val row = GIO.workgroupId.x
      val shared = GShared[Float32](BLOCK_SIZE)

      val rowSizeVal: Int32 = rowSize
      val epsVal: Float32 = eps
      val weightOffsetVal: Int32 = weightOffset
      val baseIdx = row * rowSizeVal

      // Phase 1: Each thread sums its strided elements (pure DSL expression)
      val localSum = GIO.foldRepeat[Float32](numIterations, 0.0f) { (iter, sum) =>
        val col: Int32 = tid + iter * BLOCK_SIZE
        GIO.branch(
          col < rowSizeVal, {
            val x = GIO.read(layout.input.focus(_.at(baseIdx + col))).asFloat32
            sum + (x * x)
          },
          sum,
        )
      }

      // Write local sum to shared memory
      GIO.write(shared.focus(_.at(tid)), localSum)
      GIO.barrier

      // Tree reduction: 9 levels for 512 threads
      reduceShared(shared, tid, 256)
      reduceShared(shared, tid, 128)
      reduceShared(shared, tid, 64)
      reduceShared(shared, tid, 32)
      reduceShared(shared, tid, 16)
      reduceShared(shared, tid, 8)
      reduceShared(shared, tid, 4)
      reduceShared(shared, tid, 2)
      reduceShared(shared, tid, 1)

      // Phase 3: Compute scale and write output
      {
        val totalSum = GIO.read(shared.focus(_.at(0)))
        val scale: Float32 = 1.0f / sqrt((totalSum / rowSizeVal.asFloat) + epsVal)

        GIO.repeat(numIterations): iter =>
          val col = tid + iter * BLOCK_SIZE
          GIO.when(col < rowSizeVal):
            val x = GIO.read(layout.input.focus(_.at(baseIdx + col))).asFloat32
            val w = GIO.read(layout.weight.focus(_.at(weightOffsetVal + col))).asFloat32
            GIO.write(layout.output.focus(_.at(baseIdx + col)), (x * scale * w).asFloat16)
      }

  // Helper for one level of tree reduction
  private def reduceShared(shared: GShared[Float32], tid: Int32, stride: Int)(using GIO): Unit =
    val strideVal: Int32 = stride
    GIO.when(tid < strideVal):
      val current = GIO.read(shared.focus(_.at(tid)))
      val other = GIO.read(shared.focus(_.at(tid + strideVal)))
      GIO.write(shared.focus(_.at(tid)), current + other)
    GIO.barrier
