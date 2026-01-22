package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** Fused F16 Gate + Up + SwiGLU in single dispatch.
  *
  * Computes the FFN gated activation in one pass:
  *   gate = input @ Wgate
  *   up = input @ Wup
  *   output = silu(gate) * up
  *
  * Where silu(x) = x * sigmoid(x)
  *
  * Reduces 3 dispatches → 1 by:
  *   - Computing gate and up projections together
  *   - Applying SwiGLU activation immediately after
  *
  * Each output element requires computing both gate and up for that position.
  */
object F16FusedGateUpSwiGLUProgram:
  val WARP_SIZE = 32
  val WARPS_PER_WORKGROUP = 8
  val BLOCK_SIZE = WARP_SIZE * WARPS_PER_WORKGROUP

  case class Sizes(
    batchSize: Int,
    inFeatures: Int,      // C (hidden size)
    outFeatures: Int,     // FFN (intermediate size)
    gateOffsetVec4: Int,
    upOffsetVec4: Int,
    totalGateVec4: Int,
    totalUpVec4: Int,
  ):
    require(inFeatures % 4 == 0, s"inFeatures ($inFeatures) must be divisible by 4")
    def inFeaturesDiv4: Int = inFeatures / 4
    def totalOutputs: Int = batchSize * outFeatures
    def numWorkgroups: Int = (totalOutputs + WARPS_PER_WORKGROUP - 1) / WARPS_PER_WORKGROUP
    def numVecIterations: Int = (inFeaturesDiv4 + WARP_SIZE - 1) / WARP_SIZE

  case class ProgramLayout(
    wgate: GBuffer[Vec4[Float16]],
    wup: GBuffer[Vec4[Float16]],
    input: GBuffer[Float16],
    output: GBuffer[Float16],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val inFeatures = sizes.inFeatures
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val gateOffsetVec4 = sizes.gateOffsetVec4
    val upOffsetVec4 = sizes.upOffsetVec4
    val numVecIterations = sizes.numVecIterations

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        wgate = GBuffer[Vec4[Float16]](s.totalGateVec4),
        wup = GBuffer[Vec4[Float16]](s.totalUpVec4),
        input = GBuffer[Float16](s.batchSize * s.inFeatures),
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
      val gateOffsetVec4Val: Int32 = gateOffsetVec4
      val upOffsetVec4Val: Int32 = upOffsetVec4
      val totalOutputsVal: Int32 = sizes.totalOutputs

      val outputIdx = workgroupId * WARPS_PER_WORKGROUP + warpId
      
      GIO.when(outputIdx < totalOutputsVal):
        val batch = outputIdx / outFeaturesVal
        val outIdx = outputIdx.mod(outFeaturesVal)
        
        // Compute gate dot product
        val gateLocalSum = GSeq
          .gen[Int32](laneId, _ + WARP_SIZE)
          .limit(numVecIterations)
          .unroll
          .fold(0.0f, (sum: Float32, k: Int32) =>
            when(k < inFeaturesDiv4Val):
              val gVec = GIO.read[Vec4[Float16]](layout.wgate, gateOffsetVec4Val + outIdx * inFeaturesDiv4Val + k)
              val inputBase = batch * inFeaturesVal + k * 4
              val x0 = GIO.read[Float16](layout.input, inputBase).asFloat32
              val x1 = GIO.read[Float16](layout.input, inputBase + 1).asFloat32
              val x2 = GIO.read[Float16](layout.input, inputBase + 2).asFloat32
              val x3 = GIO.read[Float16](layout.input, inputBase + 3).asFloat32
              sum + gVec.x.asFloat32 * x0 + gVec.y.asFloat32 * x1 + gVec.z.asFloat32 * x2 + gVec.w.asFloat32 * x3
            .otherwise(sum)
          )
        
        // Compute up dot product
        val upLocalSum = GSeq
          .gen[Int32](laneId, _ + WARP_SIZE)
          .limit(numVecIterations)
          .unroll
          .fold(0.0f, (sum: Float32, k: Int32) =>
            when(k < inFeaturesDiv4Val):
              val uVec = GIO.read[Vec4[Float16]](layout.wup, upOffsetVec4Val + outIdx * inFeaturesDiv4Val + k)
              val inputBase = batch * inFeaturesVal + k * 4
              val x0 = GIO.read[Float16](layout.input, inputBase).asFloat32
              val x1 = GIO.read[Float16](layout.input, inputBase + 1).asFloat32
              val x2 = GIO.read[Float16](layout.input, inputBase + 2).asFloat32
              val x3 = GIO.read[Float16](layout.input, inputBase + 3).asFloat32
              sum + uVec.x.asFloat32 * x0 + uVec.y.asFloat32 * x1 + uVec.z.asFloat32 * x2 + uVec.w.asFloat32 * x3
            .otherwise(sum)
          )

        // Reduce across warp
        val gate = GIO.subgroupAdd(gateLocalSum)
        val up = GIO.subgroupAdd(upLocalSum)
        
        // SwiGLU: silu(gate) * up = gate * sigmoid(gate) * up
        val sigmoidGate = 1.0f / (1.0f + exp(-gate))
        val result = gate * sigmoidGate * up
        
        GIO.write[Float16](layout.output, outputIdx, result.asFloat16)
