package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GShared}
import io.computenode.cyfra.dsl.gio.GIO

/** F16 matrix-vector multiply with Vec4-packed weights.
  *
  * Uses Vec4[Float16] weights for 4x memory bandwidth while keeping scalar input.
  * Optimal for activation-weight multiplies where weights are static but activations vary.
  *
  * Optimized to compute 2 output rows in PARALLEL within the same loop iteration,
  * reading input once and reusing for both rows (like llama.cpp).
  *
  * Uses 128 threads (4 warps) with shared memory reduction for better occupancy.
  *
  * @note Requires `inFeatures` divisible by 4 for Vec4 alignment.
  */
object F16MatmulVecHybridProgram:
  val WARP_SIZE = 32
  val NUM_WARPS = 4
  val NUM_ROWS = 4 // Each workgroup computes 4 output rows in parallel (was 2)
  val BLOCK_SIZE = WARP_SIZE * NUM_WARPS // 128 threads

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
    // Exact iterations - no bounds check needed if inFeatures is multiple of (BLOCK_SIZE * 4)
    def numVecIterations: Int = inFeaturesDiv4 / BLOCK_SIZE
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
    // Compile-time constants (embedded in shader as literals)
    val inFeatures = sizes.inFeatures
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val weightOffsetVec4 = sizes.weightOffsetVec4
    val numVecIterations = sizes.numVecIterations
    val totalOutputs = sizes.totalOutputs

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
      val laneId: Int32 = tid.mod(WARP_SIZE)
      val warpId: Int32 = tid / WARP_SIZE
      
      // Shared memory for partial sums from each warp (4 rows per workgroup)
      val sharedSums = GShared[Vec4[Float32]](NUM_WARPS)

      // Use for-comprehension with GIO.pure to force index computations BEFORE the loop
      for
        // Pre-compute all indices outside the hot loop
        firstRow <- GIO.pure(workgroupId * NUM_ROWS)
        batch <- GIO.pure(firstRow / outFeatures)
        inputBase <- GIO.pure(batch * inFeatures)
        outIdx0 <- GIO.pure(firstRow - batch * outFeatures)
        outIdx1 <- GIO.pure(when(outIdx0 + 1 < outFeatures)(outIdx0 + 1).otherwise(0))
        outIdx2 <- GIO.pure(when(outIdx0 + 2 < outFeatures)(outIdx0 + 2).otherwise(0))
        outIdx3 <- GIO.pure(when(outIdx0 + 3 < outFeatures)(outIdx0 + 3).otherwise(0))
        weightOffset <- GIO.pure(weightOffsetVec4: Int32)
        inFeatDiv4 <- GIO.pure(inFeaturesDiv4: Int32)
        wBase0 <- GIO.pure(weightOffset + outIdx0 * inFeatDiv4)
        wBase1 <- GIO.pure(weightOffset + outIdx1 * inFeatDiv4)
        wBase2 <- GIO.pure(weightOffset + outIdx2 * inFeatDiv4)
        wBase3 <- GIO.pure(weightOffset + outIdx3 * inFeatDiv4)
        
        // Hot loop - NO BOUNDS CHECK (assume inFeatures is multiple of BLOCK_SIZE*4)
        // This matches llama.cpp's fast path pattern
        localSums <- GIO.pure(GSeq
          .gen[Int32](tid, _ + BLOCK_SIZE)
          .limit(numVecIterations)
          .unroll
          .fold(vec4(0.0f, 0.0f, 0.0f, 0.0f), (acc: Vec4[Float32], k: Int32) =>
            // Read 4 input values and construct Vec4
            val iBase = inputBase + k * 4
            val xVec = vec4(
              GIO.read[Float16](layout.input, iBase).asFloat32,
              GIO.read[Float16](layout.input, iBase + 1).asFloat32,
              GIO.read[Float16](layout.input, iBase + 2).asFloat32,
              GIO.read[Float16](layout.input, iBase + 3).asFloat32,
            )
            
            // Read weights for all 4 rows
            val wVec0 = GIO.read[Vec4[Float16]](layout.weight, wBase0 + k)
            val wVec1 = GIO.read[Vec4[Float16]](layout.weight, wBase1 + k)
            val wVec2 = GIO.read[Vec4[Float16]](layout.weight, wBase2 + k)
            val wVec3 = GIO.read[Vec4[Float16]](layout.weight, wBase3 + k)
            
            // Compute all 4 dot products using vec4 operations
            vec4(
              acc.x + wVec0.asVec4F32.dot(xVec),
              acc.y + wVec1.asVec4F32.dot(xVec),
              acc.z + wVec2.asVec4F32.dot(xVec),
              acc.w + wVec3.asVec4F32.dot(xVec),
            )
          ))

        // First reduce within each warp using subgroupAdd
        warpSum <- GIO.pure(vec4(
          GIO.subgroupAdd(localSums.x),
          GIO.subgroupAdd(localSums.y),
          GIO.subgroupAdd(localSums.z),
          GIO.subgroupAdd(localSums.w),
        ))
        
        // Lane 0 of each warp writes to shared memory
        _ <- GIO.when(laneId === 0):
               sharedSums.write(warpId, warpSum)
        _ <- GIO.barrier
        
        // All threads read shared memory (but only tid=0 will write)
        sum0 <- GIO.pure(sharedSums.read(0))
        sum1 <- GIO.pure(sharedSums.read(1))
        sum2 <- GIO.pure(sharedSums.read(2))
        sum3 <- GIO.pure(sharedSums.read(3))
        total <- GIO.pure(vec4(
          sum0.x + sum1.x + sum2.x + sum3.x,
          sum0.y + sum1.y + sum2.y + sum3.y,
          sum0.z + sum1.z + sum2.z + sum3.z,
          sum0.w + sum1.w + sum2.w + sum3.w,
        ))
        
        // Thread 0 writes output for all 4 rows
        _ <- GIO.when(tid === 0):
               GIO.write[Float16](layout.output, firstRow, total.x.asFloat16)
        _ <- GIO.when(tid === 0 && firstRow + 1 < totalOutputs):
               GIO.write[Float16](layout.output, firstRow + 1, total.y.asFloat16)
        _ <- GIO.when(tid === 0 && firstRow + 2 < totalOutputs):
               GIO.write[Float16](layout.output, firstRow + 2, total.z.asFloat16)
        _ <- GIO.when(tid === 0 && firstRow + 3 < totalOutputs):
               GIO.write[Float16](layout.output, firstRow + 3, total.w.asFloat16)
      yield GStruct.Empty()

  /** Optimized forward with Vec4 input reads - 4x fewer memory transactions. */
  def forwardVec4(sizes: Sizes): GProgram[Sizes, ProgramLayoutVec4] =
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val weightOffsetVec4 = sizes.weightOffsetVec4
    val numVecIterations = sizes.numVecIterations
    val totalOutputs = sizes.totalOutputs

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
      val laneId: Int32 = tid.mod(WARP_SIZE)
      val warpId: Int32 = tid / WARP_SIZE
      
      val sharedSums = GShared[Vec4[Float32]](NUM_WARPS)

      for
        // Pre-compute all indices outside the hot loop
        firstRow <- GIO.pure(workgroupId * NUM_ROWS)
        batch <- GIO.pure(firstRow / outFeatures)
        inputBase <- GIO.pure(batch * inFeaturesDiv4)
        outIdx0 <- GIO.pure(firstRow - batch * outFeatures)
        outIdx1 <- GIO.pure(when(outIdx0 + 1 < outFeatures)(outIdx0 + 1).otherwise(0))
        outIdx2 <- GIO.pure(when(outIdx0 + 2 < outFeatures)(outIdx0 + 2).otherwise(0))
        outIdx3 <- GIO.pure(when(outIdx0 + 3 < outFeatures)(outIdx0 + 3).otherwise(0))
        weightOffset <- GIO.pure(weightOffsetVec4: Int32)
        inFeatDiv4 <- GIO.pure(inFeaturesDiv4: Int32)
        wBase0 <- GIO.pure(weightOffset + outIdx0 * inFeatDiv4)
        wBase1 <- GIO.pure(weightOffset + outIdx1 * inFeatDiv4)
        wBase2 <- GIO.pure(weightOffset + outIdx2 * inFeatDiv4)
        wBase3 <- GIO.pure(weightOffset + outIdx3 * inFeatDiv4)
        
        // Hot loop - NO BOUNDS CHECK
        localSums <- GIO.pure(GSeq
          .gen[Int32](tid, _ + BLOCK_SIZE)
          .limit(numVecIterations)
          .unroll
          .fold(vec4(0.0f, 0.0f, 0.0f, 0.0f), (acc: Vec4[Float32], k: Int32) =>
            // Read input ONCE as Vec4
            val xVec = GIO.read[Vec4[Float16]](layout.input, inputBase + k).asVec4F32
            
            // Read weights for all 4 rows
            val wVec0 = GIO.read[Vec4[Float16]](layout.weight, wBase0 + k).asVec4F32
            val wVec1 = GIO.read[Vec4[Float16]](layout.weight, wBase1 + k).asVec4F32
            val wVec2 = GIO.read[Vec4[Float16]](layout.weight, wBase2 + k).asVec4F32
            val wVec3 = GIO.read[Vec4[Float16]](layout.weight, wBase3 + k).asVec4F32
            
            vec4(
              acc.x + wVec0.dot(xVec),
              acc.y + wVec1.dot(xVec),
              acc.z + wVec2.dot(xVec),
              acc.w + wVec3.dot(xVec),
            )
          ))

        // First reduce within each warp
        warpSum <- GIO.pure(vec4(
          GIO.subgroupAdd(localSums.x),
          GIO.subgroupAdd(localSums.y),
          GIO.subgroupAdd(localSums.z),
          GIO.subgroupAdd(localSums.w),
        ))
        
        // Lane 0 of each warp writes to shared memory
        _ <- GIO.when(laneId === 0):
               sharedSums.write(warpId, warpSum)
        _ <- GIO.barrier
        
        // All threads read shared memory (but only tid=0 will write)
        sum0 <- GIO.pure(sharedSums.read(0))
        sum1 <- GIO.pure(sharedSums.read(1))
        sum2 <- GIO.pure(sharedSums.read(2))
        sum3 <- GIO.pure(sharedSums.read(3))
        total <- GIO.pure(vec4(
          sum0.x + sum1.x + sum2.x + sum3.x,
          sum0.y + sum1.y + sum2.y + sum3.y,
          sum0.z + sum1.z + sum2.z + sum3.z,
          sum0.w + sum1.w + sum2.w + sum3.w,
        ))
        
        // Thread 0 writes output for all 4 rows
        _ <- GIO.when(tid === 0):
               GIO.write[Float16](layout.output, firstRow, total.x.asFloat16)
        _ <- GIO.when(tid === 0 && firstRow + 1 < totalOutputs):
               GIO.write[Float16](layout.output, firstRow + 1, total.y.asFloat16)
        _ <- GIO.when(tid === 0 && firstRow + 2 < totalOutputs):
               GIO.write[Float16](layout.output, firstRow + 2, total.z.asFloat16)
        _ <- GIO.when(tid === 0 && firstRow + 3 < totalOutputs):
               GIO.write[Float16](layout.output, firstRow + 3, total.w.asFloat16)
      yield GStruct.Empty()
