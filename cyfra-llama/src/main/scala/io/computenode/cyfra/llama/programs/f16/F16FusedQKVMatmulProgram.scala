package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.{*, given}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO

/** Fused F16 Q/K/V projection in single dispatch.
  *
  * Computes all three attention projections at once:
  *   - Q = input @ Wq  (outFeatures = C)
  *   - K = input @ Wk  (outFeatures = kvSize)
  *   - V = input @ Wv  (outFeatures = kvSize)
  *
  * Reduces 3 dispatches → 1, sharing the same input read.
  * Each warp computes one output element from Q, K, or V.
  */
object F16FusedQKVMatmulProgram:
  val WARP_SIZE = 32
  val WARPS_PER_WORKGROUP = 8
  val BLOCK_SIZE = WARP_SIZE * WARPS_PER_WORKGROUP

  case class Sizes(
    batchSize: Int,
    inFeatures: Int,      // C (hidden size)
    qOutFeatures: Int,    // C (for Q)
    kvOutFeatures: Int,   // kvSize = NKV * headSize (for K and V)
    wqOffsetVec4: Int,
    wkOffsetVec4: Int,
    wvOffsetVec4: Int,
    totalWqVec4: Int,
    totalWkVec4: Int,
    totalWvVec4: Int,
  ):
    require(inFeatures % 4 == 0, s"inFeatures ($inFeatures) must be divisible by 4")
    def inFeaturesDiv4: Int = inFeatures / 4
    def totalQOutputs: Int = batchSize * qOutFeatures
    def totalKOutputs: Int = batchSize * kvOutFeatures
    def totalVOutputs: Int = batchSize * kvOutFeatures
    def totalOutputs: Int = totalQOutputs + totalKOutputs + totalVOutputs
    def numWorkgroups: Int = (totalOutputs + WARPS_PER_WORKGROUP - 1) / WARPS_PER_WORKGROUP
    def numVecIterations: Int = (inFeaturesDiv4 + WARP_SIZE - 1) / WARP_SIZE

  case class ProgramLayout(
    wq: GBuffer[Vec4[Float16]],
    wk: GBuffer[Vec4[Float16]],
    wv: GBuffer[Vec4[Float16]],
    input: GBuffer[Float16],
    q: GBuffer[Float16],
    k: GBuffer[Float16],
    v: GBuffer[Float16],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    given Sizes = sizes
    val inFeatures = sizes.inFeatures
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val qOutFeatures = sizes.qOutFeatures
    val kvOutFeatures = sizes.kvOutFeatures
    val wqOffsetVec4 = sizes.wqOffsetVec4
    val wkOffsetVec4 = sizes.wkOffsetVec4
    val wvOffsetVec4 = sizes.wvOffsetVec4
    val numVecIterations = sizes.numVecIterations
    val totalQOutputs = sizes.totalQOutputs
    val totalKOutputs = sizes.totalKOutputs

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        wq = GBuffer.sized[Vec4[Float16]](s.totalWqVec4),
        wk = GBuffer.sized[Vec4[Float16]](s.totalWkVec4),
        wv = GBuffer.sized[Vec4[Float16]](s.totalWvVec4),
        input = GBuffer.sized[Float16](s.batchSize * s.inFeatures),
        q = GBuffer.sized[Float16](s.totalQOutputs),
        k = GBuffer.sized[Float16](s.totalKOutputs),
        v = GBuffer.sized[Float16](s.totalVOutputs),
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
      val qOutFeaturesVal: Int32 = qOutFeatures
      val kvOutFeaturesVal: Int32 = kvOutFeatures
      val wqOffsetVec4Val: Int32 = wqOffsetVec4
      val wkOffsetVec4Val: Int32 = wkOffsetVec4
      val wvOffsetVec4Val: Int32 = wvOffsetVec4
      val totalQOutputsVal: Int32 = totalQOutputs
      val totalKOutputsVal: Int32 = totalKOutputs
      val totalOutputsVal: Int32 = sizes.totalOutputs

      val globalOutputIdx = workgroupId * WARPS_PER_WORKGROUP + warpId
      
      // Determine which output (Q, K, or V) this warp handles
      // Q: indices [0, totalQOutputs)
      // K: indices [totalQOutputs, totalQOutputs + totalKOutputs)
      // V: indices [totalQOutputs + totalKOutputs, total)
      
      val isQ = globalOutputIdx < totalQOutputsVal
      val isK = !isQ && (globalOutputIdx < totalQOutputsVal + totalKOutputsVal)
      val isV = !isQ && !isK
      
      // Helper function to compute matmul for a given buffer and offset
      def computeMatmul(
        weightBuffer: GBuffer[Vec4[Float16]],
        weightOffset: Int32,
        outFeatures: Int32,
        localIdx: Int32,
      ): Float32 =
        val batch = localIdx / outFeatures
        val outIdx = localIdx.mod(outFeatures)
        val localSum = GSeq
          .gen[Int32](laneId, _ + WARP_SIZE)
          .limit(numVecIterations)
          .unroll
          .fold(0.0f, (sum: Float32, k: Int32) =>
            when(k < inFeaturesDiv4Val):
              val wVec = GIO.read[Vec4[Float16]](weightBuffer, weightOffset + outIdx * inFeaturesDiv4Val + k)
              val inputBase = batch * inFeaturesVal + k * 4
              val x0 = GIO.read[Float16](layout.input, inputBase).asFloat32
              val x1 = GIO.read[Float16](layout.input, inputBase + 1).asFloat32
              val x2 = GIO.read[Float16](layout.input, inputBase + 2).asFloat32
              val x3 = GIO.read[Float16](layout.input, inputBase + 3).asFloat32
              sum + wVec.x.asFloat32 * x0 + wVec.y.asFloat32 * x1 + wVec.z.asFloat32 * x2 + wVec.w.asFloat32 * x3
            .otherwise(sum)
          )
        GIO.subgroupAdd(localSum)
      
      // Process Q outputs
      for
        _ <- GIO.when(isQ && globalOutputIdx < totalOutputsVal):
          val localIdx = globalOutputIdx
          val result = computeMatmul(layout.wq, wqOffsetVec4Val, qOutFeaturesVal, localIdx)
          GIO.write[Float16](layout.q, localIdx, result.asFloat16)
        // Process K outputs
        _ <- GIO.when(isK && globalOutputIdx < totalOutputsVal):
          val localIdx = globalOutputIdx - totalQOutputsVal
          val result = computeMatmul(layout.wk, wkOffsetVec4Val, kvOutFeaturesVal, localIdx)
          GIO.write[Float16](layout.k, localIdx, result.asFloat16)
        // Process V outputs
        _ <- GIO.when(isV && globalOutputIdx < totalOutputsVal):
          val localIdx = globalOutputIdx - totalQOutputsVal - totalKOutputsVal
          val result = computeMatmul(layout.wv, wvOffsetVec4Val, kvOutFeaturesVal, localIdx)
          GIO.write[Float16](layout.v, localIdx, result.asFloat16)
      yield GStruct.Empty()
