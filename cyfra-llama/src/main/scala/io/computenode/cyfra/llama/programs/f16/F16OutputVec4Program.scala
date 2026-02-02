package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct

/** F16 output projection with Vec4-packed weights.
  *
  * Projects hidden states to vocabulary logits using Vec4[Float16] weights.
  * Output is F32 for softmax numerical stability.
  *
  * Optimized with NUM_ROWS=4: each workgroup computes 4 output rows.
  * For vocab=32000: 8000 workgroups - balances parallelism vs dispatch overhead.
  *
  * @note Requires `hiddenSize` divisible by 4 for Vec4 alignment.
  */
object F16OutputVec4Program:
  val WARP_SIZE = 32
  val NUM_ROWS = 4 // Each workgroup computes 4 rows
  val BLOCK_SIZE = WARP_SIZE // Single warp per workgroup

  case class Sizes(batchSize: Int, hiddenSize: Int, vocabSize: Int):
    require(hiddenSize % 4 == 0, s"hiddenSize ($hiddenSize) must be divisible by 4")
    def hiddenSizeDiv4: Int = hiddenSize / 4
    def totalOutputs: Int = batchSize * vocabSize
    def numWorkgroups: Int = (totalOutputs + NUM_ROWS - 1) / NUM_ROWS
    def numVecIterations: Int = (hiddenSizeDiv4 + WARP_SIZE - 1) / WARP_SIZE

  case class ProgramLayout(
    input: GBuffer[Float16],
    weight: GBuffer[Vec4[Float16]],
    output: GBuffer[Float32],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val hiddenSize = sizes.hiddenSize
    val hiddenSizeDiv4 = sizes.hiddenSizeDiv4
    val vocabSize = sizes.vocabSize
    val numVecIterations = sizes.numVecIterations

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        input = GBuffer[Float16](s.batchSize * s.hiddenSize),
        weight = GBuffer[Vec4[Float16]](s.vocabSize * s.hiddenSizeDiv4),
        output = GBuffer[Float32](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val hiddenSizeVal: Int32 = hiddenSize
      val hiddenSizeDiv4Val: Int32 = hiddenSizeDiv4
      val vocabSizeVal: Int32 = vocabSize
      val totalOutputsVal: Int32 = sizes.totalOutputs

      // Each workgroup computes NUM_ROWS consecutive output rows
      val firstRow = workgroupId * NUM_ROWS
      val batch = firstRow / vocabSizeVal
      val inputBase0 = batch * hiddenSizeVal

      // Helper to compute one row's dot product
      def computeRow(vocabIdx: Int32): Float32 =
        val localSum = GSeq
          .gen[Int32](tid, _ + WARP_SIZE)
          .limit(numVecIterations)
          .unroll
          .fold(0.0f, (sum: Float32, k: Int32) =>
            when(k < hiddenSizeDiv4Val):
              val wVec = GIO.read[Vec4[Float16]](layout.weight, vocabIdx * hiddenSizeDiv4Val + k)
              val inputBase = inputBase0 + k * 4
              val x0 = GIO.read[Float16](layout.input, inputBase).asFloat32
              val x1 = GIO.read[Float16](layout.input, inputBase + 1).asFloat32
              val x2 = GIO.read[Float16](layout.input, inputBase + 2).asFloat32
              val x3 = GIO.read[Float16](layout.input, inputBase + 3).asFloat32
              sum + wVec.x.asFloat32 * x0 + wVec.y.asFloat32 * x1 + wVec.z.asFloat32 * x2 + wVec.w.asFloat32 * x3
            .otherwise(sum)
          )
        GIO.subgroupAdd(localSum)

      // Compute all 4 rows
      val vocabIdx0 = firstRow.mod(vocabSizeVal)
      val vocabIdx1 = (firstRow + 1).mod(vocabSizeVal)
      val vocabIdx2 = (firstRow + 2).mod(vocabSizeVal)
      val vocabIdx3 = (firstRow + 3).mod(vocabSizeVal)

      val sum0 = computeRow(vocabIdx0)
      val sum1 = computeRow(vocabIdx1)
      val sum2 = computeRow(vocabIdx2)
      val sum3 = computeRow(vocabIdx3)

      // Write results
      for
        _ <- GIO.when(firstRow < totalOutputsVal):
          GIO.write[Float32](layout.output, firstRow, sum0)
        _ <- GIO.when(firstRow + 1 < totalOutputsVal):
          GIO.write[Float32](layout.output, firstRow + 1, sum1)
        _ <- GIO.when(firstRow + 2 < totalOutputsVal):
          GIO.write[Float32](layout.output, firstRow + 2, sum2)
        _ <- GIO.when(firstRow + 3 < totalOutputsVal):
          GIO.write[Float32](layout.output, firstRow + 3, sum3)
      yield GStruct.Empty()
