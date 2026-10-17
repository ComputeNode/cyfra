package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.dsl.direct.GIO

/** F16 output projection with Vec4-packed weights.
  *
  * Projects hidden states to vocabulary logits using Vec4[Float16] weights. Output is F32 for softmax numerical stability.
  *
  * Optimized with NUM_ROWS=4: each workgroup computes 4 output rows.
  *
  * @note
  *   Requires `hiddenSize` divisible by 4 for Vec4 alignment.
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

  case class ProgramLayout(input: GBuffer[RuntimeArray[Float16]], weight: GBuffer[RuntimeArray[Vec4[Float16]]], output: GBuffer[RuntimeArray[Float32]])
      derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val hiddenSize = sizes.hiddenSize
    val hiddenSizeDiv4 = sizes.hiddenSizeDiv4
    val vocabSize = sizes.vocabSize
    val numVecIterations = sizes.numVecIterations

    GioProgram[Sizes, ProgramLayout](
      layout = s =>
        ProgramLayout(
          input = GBuffer[RuntimeArray[Float16]](s.batchSize * s.hiddenSize),
          weight = GBuffer[RuntimeArray[Vec4[Float16]]](s.vocabSize * s.hiddenSizeDiv4),
          output = GBuffer[RuntimeArray[Float32]](s.totalOutputs),
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
      def computeRow(vocabIdx: Int32)(using GIO): Float32 =
        val localSum = GIO.foldRepeat[Float32](numVecIterations, 0.0f, unroll = true) { (iter, sum) =>
          val k: Int32 = tid + iter * WARP_SIZE
          GIO.branch(
            k < hiddenSizeDiv4Val, {
              val wVec = GIO.read(layout.weight.focus(_.at(vocabIdx * hiddenSizeDiv4Val + k)))
              val inputBase = inputBase0 + k * 4
              val x0 = GIO.read(layout.input.focus(_.at(inputBase))).asFloat32
              val x1 = GIO.read(layout.input.focus(_.at(inputBase + 1))).asFloat32
              val x2 = GIO.read(layout.input.focus(_.at(inputBase + 2))).asFloat32
              val x3 = GIO.read(layout.input.focus(_.at(inputBase + 3))).asFloat32
              sum + wVec.x.asFloat32 * x0 + wVec.y.asFloat32 * x1 + wVec.z.asFloat32 * x2 + wVec.w.asFloat32 * x3
            },
            sum,
          )
        }
        GIO.subgroupAdd(localSum)

      // Compute all 4 rows
      val vocabIdx0 = firstRow % vocabSizeVal
      val vocabIdx1 = (firstRow + 1) % vocabSizeVal
      val vocabIdx2 = (firstRow + 2) % vocabSizeVal
      val vocabIdx3 = (firstRow + 3) % vocabSizeVal

      val sum0 = computeRow(vocabIdx0)
      val sum1 = computeRow(vocabIdx1)
      val sum2 = computeRow(vocabIdx2)
      val sum3 = computeRow(vocabIdx3)

      // Write results
      GIO.when(firstRow < totalOutputsVal):
        GIO.write(layout.output.focus(_.at(firstRow)), sum0)
      GIO.when(firstRow + 1 < totalOutputsVal):
        GIO.write(layout.output.focus(_.at(firstRow + 1)), sum1)
      GIO.when(firstRow + 2 < totalOutputsVal):
        GIO.write(layout.output.focus(_.at(firstRow + 2)), sum2)
      GIO.when(firstRow + 3 < totalOutputsVal):
        GIO.write(layout.output.focus(_.at(firstRow + 3)), sum3)
