package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.core.memory.{GBuffer, GShared}
import io.computenode.cyfra.dsl.direct.GIO

/** F16 matrix-vector multiply with Vec4-packed weights.
  *
  * Uses Vec4[Float16] weights for 4x memory bandwidth while keeping scalar input. Optimal for activation-weight multiplies where weights are static
  * but activations vary.
  *
  * Optimized to compute 2 output rows in PARALLEL within the same loop iteration, reading input once and reusing for both rows (like llama.cpp).
  *
  * Uses 128 threads (4 warps) with shared memory reduction for better occupancy.
  *
  * @note
  *   Requires `inFeatures` divisible by 4 for Vec4 alignment.
  */
object F16MatmulVecHybridProgram:
  val WARP_SIZE = 32
  val NUM_WARPS = 4
  val NUM_ROWS = 4 // Each workgroup computes 4 output rows in parallel (was 2)
  val BLOCK_SIZE = WARP_SIZE * NUM_WARPS // 128 threads

  case class Sizes(batchSize: Int, inFeatures: Int, outFeatures: Int, weightOffsetVec4: Int = 0, totalWeightVec4: Int = -1):
    require(inFeatures % 4 == 0, s"inFeatures ($inFeatures) must be divisible by 4")
    def inFeaturesDiv4: Int = inFeatures / 4
    def totalOutputs: Int = batchSize * outFeatures
    def numWorkgroups: Int = (totalOutputs + NUM_ROWS - 1) / NUM_ROWS
    // Exact iterations - no bounds check needed if inFeatures is multiple of (BLOCK_SIZE * 4)
    def numVecIterations: Int = inFeaturesDiv4 / BLOCK_SIZE
    def actualWeightVec4: Int = if totalWeightVec4 < 0 then outFeatures * inFeaturesDiv4 else totalWeightVec4

  case class ProgramLayout(weight: GBuffer[GArray[Vec4[Float16]]], input: GBuffer[GArray[Float16]], output: GBuffer[GArray[Float16]]) derives Layout

  /** Layout with Vec4 input for optimal memory bandwidth. */
  case class ProgramLayoutVec4(weight: GBuffer[GArray[Vec4[Float16]]], input: GBuffer[GArray[Vec4[Float16]]], output: GBuffer[GArray[Float16]])
      derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    // Compile-time constants (embedded in shader as literals)
    val inFeatures = sizes.inFeatures
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val weightOffsetVec4 = sizes.weightOffsetVec4
    val numVecIterations = sizes.numVecIterations
    val totalOutputs = sizes.totalOutputs

    GioProgram[Sizes, ProgramLayout](
      layout = s =>
        ProgramLayout(
          weight = GBuffer[GArray[Vec4[Float16]]](s.actualWeightVec4),
          input = GBuffer[GArray[Float16]](s.batchSize * s.inFeatures),
          output = GBuffer[GArray[Float16]](s.totalOutputs),
        ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val laneId: Int32 = tid % WARP_SIZE
      val warpId: Int32 = tid / WARP_SIZE

      // Shared memory for partial sums from each warp (4 rows per workgroup)
      val sharedSums = GShared[Vec4[Float32]](NUM_WARPS)

      // Materialize indices before the hot loop.
      // Pre-compute all indices outside the hot loop
      val firstRow = GIO.eval(workgroupId * NUM_ROWS)
      val batch = GIO.eval(firstRow / outFeatures)
      val inputBase = GIO.eval(batch * inFeatures)
      val outIdx0 = GIO.eval(firstRow - batch * outFeatures)
      val outIdx1 = GIO.branch(outIdx0 + 1 < outFeatures, outIdx0 + 1, Int32(0))
      val outIdx2 = GIO.branch(outIdx0 + 2 < outFeatures, outIdx0 + 2, Int32(0))
      val outIdx3 = GIO.branch(outIdx0 + 3 < outFeatures, outIdx0 + 3, Int32(0))
      val weightOffset = GIO.eval(weightOffsetVec4: Int32)
      val inFeatDiv4 = GIO.eval(inFeaturesDiv4: Int32)
      val wBase0 = GIO.eval(weightOffset + outIdx0 * inFeatDiv4)
      val wBase1 = GIO.eval(weightOffset + outIdx1 * inFeatDiv4)
      val wBase2 = GIO.eval(weightOffset + outIdx2 * inFeatDiv4)
      val wBase3 = GIO.eval(weightOffset + outIdx3 * inFeatDiv4)

      // Hot loop - NO BOUNDS CHECK (assume inFeatures is multiple of BLOCK_SIZE*4)
      // This matches llama.cpp's fast path pattern
      val localSums = GIO.foldRepeat[Vec4[Float32]](numVecIterations, Vec4(0.0f, 0.0f, 0.0f, 0.0f), unroll = true) { (iter, acc) =>
        val k: Int32 = tid + iter * BLOCK_SIZE
        // Read 4 input values and construct Vec4
        val iBase = inputBase + k * 4
        val xVec = Vec4(
          GIO.read(layout.input.focus(_.at(iBase))).asFloat32,
          GIO.read(layout.input.focus(_.at(iBase + 1))).asFloat32,
          GIO.read(layout.input.focus(_.at(iBase + 2))).asFloat32,
          GIO.read(layout.input.focus(_.at(iBase + 3))).asFloat32,
        )

        // Read weights for all 4 rows
        val wVec0 = GIO.read(layout.weight.focus(_.at(wBase0 + k)))
        val wVec1 = GIO.read(layout.weight.focus(_.at(wBase1 + k)))
        val wVec2 = GIO.read(layout.weight.focus(_.at(wBase2 + k)))
        val wVec3 = GIO.read(layout.weight.focus(_.at(wBase3 + k)))

        // Compute all 4 dot products using vec4 operations
        Vec4(acc.x + wVec0.asFloat32.dot(xVec), acc.y + wVec1.asFloat32.dot(xVec), acc.z + wVec2.asFloat32.dot(xVec), acc.w + wVec3.asFloat32.dot(xVec))
      }

      // First reduce within each warp using subgroupAdd
      val warpSum =
        GIO.eval(Vec4(GIO.subgroupAdd(localSums.x), GIO.subgroupAdd(localSums.y), GIO.subgroupAdd(localSums.z), GIO.subgroupAdd(localSums.w)))

      // Lane 0 of each warp writes to shared memory
      GIO.when(laneId === 0):
        GIO.write(sharedSums.focus(_.at(warpId)), warpSum)
      GIO.barrier

      // All threads read shared memory (but only tid=0 will write)
      val sum0 = GIO.read(sharedSums.focus(_.at(0)))
      val sum1 = GIO.read(sharedSums.focus(_.at(1)))
      val sum2 = GIO.read(sharedSums.focus(_.at(2)))
      val sum3 = GIO.read(sharedSums.focus(_.at(3)))
      val total = GIO.eval(
        Vec4(
          sum0.x + sum1.x + sum2.x + sum3.x,
          sum0.y + sum1.y + sum2.y + sum3.y,
          sum0.z + sum1.z + sum2.z + sum3.z,
          sum0.w + sum1.w + sum2.w + sum3.w,
        ),
      )

      // Thread 0 writes output for all 4 rows
      GIO.when(tid === 0):
        GIO.write(layout.output.focus(_.at(firstRow)), total.x.asFloat16)
      GIO.when(tid === 0 && firstRow + 1 < totalOutputs):
        GIO.write(layout.output.focus(_.at(firstRow + 1)), total.y.asFloat16)
      GIO.when(tid === 0 && firstRow + 2 < totalOutputs):
        GIO.write(layout.output.focus(_.at(firstRow + 2)), total.z.asFloat16)
      GIO.when(tid === 0 && firstRow + 3 < totalOutputs):
        GIO.write(layout.output.focus(_.at(firstRow + 3)), total.w.asFloat16)

  /** Optimized forward with Vec4 input reads - 4x fewer memory transactions. */
  def forwardVec4(sizes: Sizes): GProgram[Sizes, ProgramLayoutVec4] =
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val weightOffsetVec4 = sizes.weightOffsetVec4
    val numVecIterations = sizes.numVecIterations
    val totalOutputs = sizes.totalOutputs

    GioProgram[Sizes, ProgramLayoutVec4](
      layout = s =>
        ProgramLayoutVec4(
          weight = GBuffer[GArray[Vec4[Float16]]](s.actualWeightVec4),
          input = GBuffer[GArray[Vec4[Float16]]](s.batchSize * s.inFeaturesDiv4),
          output = GBuffer[GArray[Float16]](s.totalOutputs),
        ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val laneId: Int32 = tid % WARP_SIZE
      val warpId: Int32 = tid / WARP_SIZE

      val sharedSums = GShared[Vec4[Float32]](NUM_WARPS)

      // Pre-compute all indices outside the hot loop
      val firstRow = GIO.eval(workgroupId * NUM_ROWS)
      val batch = GIO.eval(firstRow / outFeatures)
      val inputBase = GIO.eval(batch * inFeaturesDiv4)
      val outIdx0 = GIO.eval(firstRow - batch * outFeatures)
      val outIdx1 = GIO.branch(outIdx0 + 1 < outFeatures, outIdx0 + 1, Int32(0))
      val outIdx2 = GIO.branch(outIdx0 + 2 < outFeatures, outIdx0 + 2, Int32(0))
      val outIdx3 = GIO.branch(outIdx0 + 3 < outFeatures, outIdx0 + 3, Int32(0))
      val weightOffset = GIO.eval(weightOffsetVec4: Int32)
      val inFeatDiv4 = GIO.eval(inFeaturesDiv4: Int32)
      val wBase0 = GIO.eval(weightOffset + outIdx0 * inFeatDiv4)
      val wBase1 = GIO.eval(weightOffset + outIdx1 * inFeatDiv4)
      val wBase2 = GIO.eval(weightOffset + outIdx2 * inFeatDiv4)
      val wBase3 = GIO.eval(weightOffset + outIdx3 * inFeatDiv4)

      // Hot loop - NO BOUNDS CHECK
      val localSums = GIO.foldRepeat[Vec4[Float32]](numVecIterations, Vec4(0.0f, 0.0f, 0.0f, 0.0f), unroll = true) { (iter, acc) =>
        val k: Int32 = tid + iter * BLOCK_SIZE
        // Read input ONCE as Vec4
        val xVec = GIO.read(layout.input.focus(_.at(inputBase + k))).asFloat32

        // Read weights for all 4 rows
        val wVec0 = GIO.read(layout.weight.focus(_.at(wBase0 + k))).asFloat32
        val wVec1 = GIO.read(layout.weight.focus(_.at(wBase1 + k))).asFloat32
        val wVec2 = GIO.read(layout.weight.focus(_.at(wBase2 + k))).asFloat32
        val wVec3 = GIO.read(layout.weight.focus(_.at(wBase3 + k))).asFloat32

        Vec4(acc.x + wVec0.dot(xVec), acc.y + wVec1.dot(xVec), acc.z + wVec2.dot(xVec), acc.w + wVec3.dot(xVec))
      }

      // First reduce within each warp
      val warpSum =
        GIO.eval(Vec4(GIO.subgroupAdd(localSums.x), GIO.subgroupAdd(localSums.y), GIO.subgroupAdd(localSums.z), GIO.subgroupAdd(localSums.w)))

      // Lane 0 of each warp writes to shared memory
      GIO.when(laneId === 0):
        GIO.write(sharedSums.focus(_.at(warpId)), warpSum)
      GIO.barrier

      // All threads read shared memory (but only tid=0 will write)
      val sum0 = GIO.read(sharedSums.focus(_.at(0)))
      val sum1 = GIO.read(sharedSums.focus(_.at(1)))
      val sum2 = GIO.read(sharedSums.focus(_.at(2)))
      val sum3 = GIO.read(sharedSums.focus(_.at(3)))
      val total = GIO.eval(
        Vec4(
          sum0.x + sum1.x + sum2.x + sum3.x,
          sum0.y + sum1.y + sum2.y + sum3.y,
          sum0.z + sum1.z + sum2.z + sum3.z,
          sum0.w + sum1.w + sum2.w + sum3.w,
        ),
      )

      // Thread 0 writes output for all 4 rows
      GIO.when(tid === 0):
        GIO.write(layout.output.focus(_.at(firstRow)), total.x.asFloat16)
      GIO.when(tid === 0 && firstRow + 1 < totalOutputs):
        GIO.write(layout.output.focus(_.at(firstRow + 1)), total.y.asFloat16)
      GIO.when(tid === 0 && firstRow + 2 < totalOutputs):
        GIO.write(layout.output.focus(_.at(firstRow + 2)), total.z.asFloat16)
      GIO.when(tid === 0 && firstRow + 3 < totalOutputs):
        GIO.write(layout.output.focus(_.at(firstRow + 3)), total.w.asFloat16)
