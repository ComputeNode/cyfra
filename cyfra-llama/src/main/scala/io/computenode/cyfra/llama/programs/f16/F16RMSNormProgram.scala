package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.binding.GShared

/** F16 Root Mean Square Layer Normalization.
  *
  * Normalizes input by RMS: `output[i] = input[i] / rms(input) * weight[i]`.
  * Accumulates in F32 for numerical precision.
  */
object F16RMSNormProgram:
  val BLOCK_SIZE = 512
  
  case class Sizes(
    numRows: Int,
    rowSize: Int,
    eps: Float,
    weightOffset: Int = 0,
    totalWeightSize: Int = -1,
  ):
    def numIterations: Int = (rowSize + BLOCK_SIZE - 1) / BLOCK_SIZE
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
      val localSum = GSeq
        .gen[Int32](tid, _ + BLOCK_SIZE)
        .limit(numIterations)
        .fold(0.0f, (sum: Float32, col: Int32) =>
          when(col < rowSizeVal):
            val x = GIO.read[Float16](layout.input, baseIdx + col).asFloat32
            sum + (x * x)
          .otherwise(sum)
        )
      
      for
        // Write local sum to shared memory
        _ <- shared.write(tid, localSum)
        _ <- GIO.barrier
        
        // Tree reduction: 9 levels for 512 threads
        _ <- reduceShared(shared, tid, 256)
        _ <- reduceShared(shared, tid, 128)
        _ <- reduceShared(shared, tid, 64)
        _ <- reduceShared(shared, tid, 32)
        _ <- reduceShared(shared, tid, 16)
        _ <- reduceShared(shared, tid, 8)
        _ <- reduceShared(shared, tid, 4)
        _ <- reduceShared(shared, tid, 2)
        _ <- reduceShared(shared, tid, 1)
        
        // Phase 3: Compute scale and write output
        _ <- {
          val totalSum = shared.read(0)
          val scale: Float32 = 1.0f / sqrt((totalSum / rowSizeVal.asFloat) + epsVal)
          
          GIO.repeat(numIterations): iter =>
            val col = tid + iter * BLOCK_SIZE
            GIO.when(col < rowSizeVal):
              val x = GIO.read[Float16](layout.input, baseIdx + col).asFloat32
              val w = GIO.read[Float16](layout.weight, weightOffsetVal + col).asFloat32
              GIO.write[Float16](layout.output, baseIdx + col, (x * scale * w).asFloat16)
        }
      yield GStruct.Empty()
  
  // Helper for one level of tree reduction
  private def reduceShared(shared: GShared[Float32], tid: Int32, stride: Int): GIO[GStruct.Empty] =
    val strideVal: Int32 = stride
    for
      _ <- GIO.when(tid < strideVal):
        val current = shared.read(tid)
        val other = shared.read(tid + strideVal)
        shared.write(tid, current + other)
      _ <- GIO.barrier
    yield GStruct.Empty()