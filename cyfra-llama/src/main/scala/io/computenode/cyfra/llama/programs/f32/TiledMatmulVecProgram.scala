package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct.Empty

/** Matrix-Vector Multiplication with subgroup reduction.
  *
  * One warp (32 threads) computes one output element using subgroup operations.
  * Multiple warps per workgroup for better occupancy.
  *
  * Performance optimizations:
  *   1. Subgroup reduction (hardware accelerated)
  *   2. Multiple warps per workgroup (8 warps = 8 outputs per workgroup)
  *   3. Strided access within each warp
  * 
  * Supports layered weights via weightOffset (defaults to 0 for standalone use).
  */
object TiledMatmulVecProgram:
  val WARP_SIZE = 32
  val WARPS_PER_WORKGROUP = 8
  val BLOCK_SIZE = WARP_SIZE * WARPS_PER_WORKGROUP // 256

  case class Sizes(
    batchSize: Int,
    inFeatures: Int,
    outFeatures: Int,
    weightOffset: Int = 0,
    totalWeightSize: Int = -1,  // -1 means use outFeatures * inFeatures (single layer)
  ):
    def totalOutputs: Int = batchSize * outFeatures
    def numWorkgroups: Int = (totalOutputs + WARPS_PER_WORKGROUP - 1) / WARPS_PER_WORKGROUP
    def numIterations: Int = (inFeatures + WARP_SIZE - 1) / WARP_SIZE
    def actualWeightSize: Int = if totalWeightSize < 0 then outFeatures * inFeatures else totalWeightSize

  case class ProgramLayout(
    weight: GBuffer[Float32],
    input: GBuffer[Float32],
    output: GBuffer[Float32],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val inFeatures = sizes.inFeatures
    val outFeatures = sizes.outFeatures
    val weightOffset = sizes.weightOffset
    val numIterations = sizes.numIterations

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        weight = GBuffer[Float32](s.actualWeightSize),
        input = GBuffer[Float32](s.batchSize * s.inFeatures),
        output = GBuffer[Float32](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val laneId = tid.mod(WARP_SIZE)
      val warpId = tid / WARP_SIZE
      val inFeaturesVal: Int32 = inFeatures
      val outFeaturesVal: Int32 = outFeatures
      val weightOffsetVal: Int32 = weightOffset
      val totalOutputsVal: Int32 = sizes.totalOutputs

      val outputIdx = workgroupId * WARPS_PER_WORKGROUP + warpId
      val batch = outputIdx / outFeaturesVal
      val outIdx = outputIdx.mod(outFeaturesVal)

      val localSum = GSeq
        .gen[Int32](laneId, _ + WARP_SIZE)
        .limit(numIterations)
        .fold(0.0f, (sum: Float32, k: Int32) =>
          when(k < inFeaturesVal):
            val w = GIO.read[Float32](layout.weight, weightOffsetVal + outIdx * inFeaturesVal + k)
            val x = GIO.read[Float32](layout.input, batch * inFeaturesVal + k)
            sum + w * x
          .otherwise(sum)
        )

      val totalSum = GIO.subgroupAdd(localSum)
      GIO.when(outputIdx < totalOutputsVal):
        GIO.write[Float32](layout.output, outputIdx, totalSum)
