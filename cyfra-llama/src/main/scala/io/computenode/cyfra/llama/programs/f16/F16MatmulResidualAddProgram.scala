package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** Fused F16 matrix-vector multiply + residual add.
  *
  * Computes: output[i] = residual[i] + dot(weight[i], input)
  *
  * Eliminates the need for separate Copy + ResidualAdd programs by:
  *   1. Reading residual during computation (no need to copy first)
  *   2. Adding residual directly to matmul result
  *
  * This reduces dispatch count by 2 per usage (Copy + ResidualAdd → nothing).
  */
object F16MatmulResidualAddProgram:
  val WARP_SIZE = 32
  val WARPS_PER_WORKGROUP = 8
  val BLOCK_SIZE = WARP_SIZE * WARPS_PER_WORKGROUP

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
    def numWorkgroups: Int = (totalOutputs + WARPS_PER_WORKGROUP - 1) / WARPS_PER_WORKGROUP
    def numVecIterations: Int = (inFeaturesDiv4 + WARP_SIZE - 1) / WARP_SIZE
    def actualWeightVec4: Int = if totalWeightVec4 < 0 then outFeatures * inFeaturesDiv4 else totalWeightVec4

  /** Layout with Vec4 input for optimal memory bandwidth. */
  case class ProgramLayoutVec4(
    weight: GBuffer[Vec4[Float16]],
    input: GBuffer[Vec4[Float16]],
    residual: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout

  /** Legacy layout with scalar input. */
  case class ProgramLayout(
    weight: GBuffer[Vec4[Float16]],
    input: GBuffer[Float16],
    residual: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout

  /** Optimized forward with Vec4 input reads. */
  def forwardVec4(sizes: Sizes): GProgram[Sizes, ProgramLayoutVec4] =
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val weightOffsetVec4 = sizes.weightOffsetVec4
    val numVecIterations = sizes.numVecIterations

    GProgram[Sizes, ProgramLayoutVec4](
      layout = s => ProgramLayoutVec4(
        weight = GBuffer[Vec4[Float16]](s.actualWeightVec4),
        input = GBuffer[Vec4[Float16]](s.batchSize * s.inFeaturesDiv4),
        residual = GBuffer[Float16](s.totalOutputs),
        output = GBuffer[Float16](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val laneId = tid.mod(WARP_SIZE)
      val warpId = tid / WARP_SIZE
      val inFeaturesDiv4Val: Int32 = inFeaturesDiv4
      val outFeaturesVal: Int32 = outFeatures
      val weightOffsetVec4Val: Int32 = weightOffsetVec4
      val totalOutputsVal: Int32 = sizes.totalOutputs

      val outputIdx = workgroupId * WARPS_PER_WORKGROUP + warpId
      val batch = outputIdx / outFeaturesVal
      val outIdx = outputIdx.mod(outFeaturesVal)

      val localSum = GSeq
        .gen[Int32](laneId, _ + WARP_SIZE)
        .limit(numVecIterations)
        .unroll
        .fold(0.0f, (sum: Float32, k: Int32) =>
          when(k < inFeaturesDiv4Val):
            val wVec = GIO.read[Vec4[Float16]](layout.weight, weightOffsetVec4Val + outIdx * inFeaturesDiv4Val + k)
            val xVec = GIO.read[Vec4[Float16]](layout.input, batch * inFeaturesDiv4Val + k)
            val wF32 = vec4(wVec.x.asFloat32, wVec.y.asFloat32, wVec.z.asFloat32, wVec.w.asFloat32)
            val xF32 = vec4(xVec.x.asFloat32, xVec.y.asFloat32, xVec.z.asFloat32, xVec.w.asFloat32)
            sum + wF32.dot(xF32)
          .otherwise(sum)
        )

      val totalSum = GIO.subgroupAdd(localSum)
      GIO.when(outputIdx < totalOutputsVal):
        val residualVal = GIO.read[Float16](layout.residual, outputIdx).asFloat32
        val result = totalSum + residualVal
        GIO.write[Float16](layout.output, outputIdx, result.asFloat16)

  /** Legacy forward with scalar input reads. */
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
        residual = GBuffer[Float16](s.totalOutputs),
        output = GBuffer[Float16](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val laneId = tid.mod(WARP_SIZE)
      val warpId = tid / WARP_SIZE
      val inFeaturesVal: Int32 = inFeatures
      val inFeaturesDiv4Val: Int32 = inFeaturesDiv4
      val outFeaturesVal: Int32 = outFeatures
      val weightOffsetVec4Val: Int32 = weightOffsetVec4
      val totalOutputsVal: Int32 = sizes.totalOutputs

      val outputIdx = workgroupId * WARPS_PER_WORKGROUP + warpId
      val batch = outputIdx / outFeaturesVal
      val outIdx = outputIdx.mod(outFeaturesVal)

      val localSum = GSeq
        .gen[Int32](laneId, _ + WARP_SIZE)
        .limit(numVecIterations)
        .unroll
        .fold(0.0f, (sum: Float32, k: Int32) =>
          when(k < inFeaturesDiv4Val):
            val wVec = GIO.read[Vec4[Float16]](layout.weight, weightOffsetVec4Val + outIdx * inFeaturesDiv4Val + k)
            val inputBase = batch * inFeaturesVal + k * 4
            val x0 = GIO.read[Float16](layout.input, inputBase).asFloat32
            val x1 = GIO.read[Float16](layout.input, inputBase + 1).asFloat32
            val x2 = GIO.read[Float16](layout.input, inputBase + 2).asFloat32
            val x3 = GIO.read[Float16](layout.input, inputBase + 3).asFloat32
            sum + wVec.x.asFloat32 * x0 + wVec.y.asFloat32 * x1 + wVec.z.asFloat32 * x2 + wVec.w.asFloat32 * x3
          .otherwise(sum)
        )

      val totalSum = GIO.subgroupAdd(localSum)
      GIO.when(outputIdx < totalOutputsVal):
        // Read residual and add to matmul result
        val residualVal = GIO.read[Float16](layout.residual, outputIdx).asFloat32
        val result = totalSum + residualVal
        GIO.write[Float16](layout.output, outputIdx, result.asFloat16)
