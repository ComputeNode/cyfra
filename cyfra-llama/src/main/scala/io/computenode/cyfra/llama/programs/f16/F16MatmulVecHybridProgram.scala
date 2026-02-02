package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** F16 matrix-vector multiply with Vec4-packed weights.
  *
  * Uses Vec4[Float16] weights for 4x memory bandwidth while keeping scalar input.
  * Optimal for activation-weight multiplies where weights are static but activations vary.
  *
  * @note Requires `inFeatures` divisible by 4 for Vec4 alignment.
  */
object F16MatmulVecHybridProgram:
  val WARP_SIZE = 32
  val NUM_ROWS = 2 // Like llama.cpp: each workgroup computes multiple output rows
  val BLOCK_SIZE = WARP_SIZE // Single warp per workgroup for better efficiency

  case class Sizes(
    batchSize: Int,
    inFeatures: Int,
    outFeatures: Int,
    weightOffsetVec4: Int = 0,
    totalWeightVec4: Int = -1,
  ):
    require(inFeatures % 4 == 0, s"inFeatures ($inFeatures) must be divisible by 4")
    def inFeaturesDiv4: Int = inFeatures / 4
    def totalOutputs: Int = batchSize * outFeatures
    def numWorkgroups: Int = (totalOutputs + NUM_ROWS - 1) / NUM_ROWS
    def numVecIterations: Int = (inFeaturesDiv4 + WARP_SIZE - 1) / WARP_SIZE
    def actualWeightVec4: Int = if totalWeightVec4 < 0 then outFeatures * inFeaturesDiv4 else totalWeightVec4

  case class ProgramLayout(
    weight: GBuffer[Vec4[Float16]],
    input: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout

  /** Layout with Vec4 input for optimal memory bandwidth. */
  case class ProgramLayoutVec4(
    weight: GBuffer[Vec4[Float16]],
    input: GBuffer[Vec4[Float16]],
    output: GBuffer[Float16],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val inFeatures = sizes.inFeatures
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val weightOffsetVec4 = sizes.weightOffsetVec4
    val numVecIterations = sizes.numVecIterations

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        weight = GBuffer[Vec4[Float16]](s.actualWeightVec4),
        input = GBuffer[Float16](s.batchSize * s.inFeatures),
        output = GBuffer[Float16](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val inFeaturesVal: Int32 = inFeatures
      val inFeaturesDiv4Val: Int32 = inFeaturesDiv4
      val outFeaturesVal: Int32 = outFeatures
      val weightOffsetVec4Val: Int32 = weightOffsetVec4
      val totalOutputsVal: Int32 = sizes.totalOutputs

      // Like llama.cpp: each workgroup computes NUM_ROWS consecutive output rows
      val firstRow = workgroupId * NUM_ROWS
      val batch = firstRow / outFeaturesVal
      val outIdx0 = firstRow.mod(outFeaturesVal)
      val outIdx1 = (firstRow + 1).mod(outFeaturesVal)
      val inputBase0 = batch * inFeaturesVal

      // Compute row 0 - always runs
      val localSum0 = GSeq
        .gen[Int32](tid, _ + WARP_SIZE)
        .limit(numVecIterations)
        .unroll
        .fold(0.0f, (sum: Float32, k: Int32) =>
          when(k < inFeaturesDiv4Val):
            val wVec = GIO.read[Vec4[Float16]](layout.weight, weightOffsetVec4Val + outIdx0 * inFeaturesDiv4Val + k)
            val inputBase = inputBase0 + k * 4
            val x0 = GIO.read[Float16](layout.input, inputBase).asFloat32
            val x1 = GIO.read[Float16](layout.input, inputBase + 1).asFloat32
            val x2 = GIO.read[Float16](layout.input, inputBase + 2).asFloat32
            val x3 = GIO.read[Float16](layout.input, inputBase + 3).asFloat32
            sum + wVec.x.asFloat32 * x0 + wVec.y.asFloat32 * x1 + wVec.z.asFloat32 * x2 + wVec.w.asFloat32 * x3
          .otherwise(sum)
        )

      // Compute row 1 - always runs (may compute garbage if out of bounds, but we won't write it)
      val localSum1 = GSeq
        .gen[Int32](tid, _ + WARP_SIZE)
        .limit(numVecIterations)
        .unroll
        .fold(0.0f, (sum: Float32, k: Int32) =>
          when(k < inFeaturesDiv4Val):
            val wVec = GIO.read[Vec4[Float16]](layout.weight, weightOffsetVec4Val + outIdx1 * inFeaturesDiv4Val + k)
            val inputBase = inputBase0 + k * 4
            val x0 = GIO.read[Float16](layout.input, inputBase).asFloat32
            val x1 = GIO.read[Float16](layout.input, inputBase + 1).asFloat32
            val x2 = GIO.read[Float16](layout.input, inputBase + 2).asFloat32
            val x3 = GIO.read[Float16](layout.input, inputBase + 3).asFloat32
            sum + wVec.x.asFloat32 * x0 + wVec.y.asFloat32 * x1 + wVec.z.asFloat32 * x2 + wVec.w.asFloat32 * x3
          .otherwise(sum)
        )

      // Reduce across the warp - all lanes get the same result
      val totalSum0 = GIO.subgroupAdd(localSum0)
      val totalSum1 = GIO.subgroupAdd(localSum1)

      // Write row 0 unconditionally, row 1 with bounds check
      // Use for-comprehension to ensure proper sequencing
      for
        _ <- GIO.write[Float16](layout.output, firstRow, totalSum0.asFloat16)
        _ <- GIO.when(firstRow + 1 < totalOutputsVal):
               GIO.write[Float16](layout.output, firstRow + 1, totalSum1.asFloat16)
      yield GStruct.Empty()

  /** Optimized forward with Vec4 input reads - 4x fewer memory transactions. */
  def forwardVec4(sizes: Sizes): GProgram[Sizes, ProgramLayoutVec4] =
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val weightOffsetVec4 = sizes.weightOffsetVec4
    val numVecIterations = sizes.numVecIterations

    GProgram[Sizes, ProgramLayoutVec4](
      layout = s => ProgramLayoutVec4(
        weight = GBuffer[Vec4[Float16]](s.actualWeightVec4),
        input = GBuffer[Vec4[Float16]](s.batchSize * s.inFeaturesDiv4),
        output = GBuffer[Float16](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val inFeaturesDiv4Val: Int32 = inFeaturesDiv4
      val outFeaturesVal: Int32 = outFeatures
      val weightOffsetVec4Val: Int32 = weightOffsetVec4
      val totalOutputsVal: Int32 = sizes.totalOutputs

      // Like llama.cpp: each workgroup computes NUM_ROWS consecutive output rows
      val firstRow = workgroupId * NUM_ROWS
      val batch = firstRow / outFeaturesVal
      val outIdx0 = firstRow.mod(outFeaturesVal)
      val outIdx1 = (firstRow + 1).mod(outFeaturesVal)
      val inputBase0 = batch * inFeaturesDiv4Val

      // Compute row 0 - always runs
      val localSum0 = GSeq
        .gen[Int32](tid, _ + WARP_SIZE)
        .limit(numVecIterations)
        .unroll
        .fold(0.0f, (sum: Float32, k: Int32) =>
          when(k < inFeaturesDiv4Val):
            val wVec = GIO.read[Vec4[Float16]](layout.weight, weightOffsetVec4Val + outIdx0 * inFeaturesDiv4Val + k)
            val xVec = GIO.read[Vec4[Float16]](layout.input, inputBase0 + k)
            sum + wVec.asVec4F32.dot(xVec.asVec4F32)
          .otherwise(sum)
        )

      // Compute row 1 - always runs
      val localSum1 = GSeq
        .gen[Int32](tid, _ + WARP_SIZE)
        .limit(numVecIterations)
        .unroll
        .fold(0.0f, (sum: Float32, k: Int32) =>
          when(k < inFeaturesDiv4Val):
            val wVec = GIO.read[Vec4[Float16]](layout.weight, weightOffsetVec4Val + outIdx1 * inFeaturesDiv4Val + k)
            val xVec = GIO.read[Vec4[Float16]](layout.input, inputBase0 + k)
            sum + wVec.asVec4F32.dot(xVec.asVec4F32)
          .otherwise(sum)
        )

      // Reduce across the warp - all lanes get the same result
      val totalSum0 = GIO.subgroupAdd(localSum0)
      val totalSum1 = GIO.subgroupAdd(localSum1)

      // Write row 0 unconditionally, row 1 with bounds check
      // Use for-comprehension to ensure proper sequencing
      for
        _ <- GIO.write[Float16](layout.output, firstRow, totalSum0.asFloat16)
        _ <- GIO.when(firstRow + 1 < totalOutputsVal):
               GIO.write[Float16](layout.output, firstRow + 1, totalSum1.asFloat16)
      yield GStruct.Empty()
