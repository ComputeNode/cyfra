package io.computenode.cyfra.llama.programs.f16

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.binding.GShared

/** Fused F16 Gate + Up + SwiGLU in single dispatch.
  *
  * Computes the FFN gated activation in one pass:
  *   gate = input @ Wgate
  *   up = input @ Wup
  *   output = silu(gate) * up
  *
  * Optimized with:
  *   - Shared memory for input (loaded once, used for both gate and up)
  *   - Interleaved gate/up computation (better register utilization)
  *   - F16 throughout with F32 accumulation for precision
  *   - 16 outputs per workgroup (instead of 8)
  */
object F16FusedGateUpSwiGLUProgram:
  val WARP_SIZE = 32
  val WARPS_PER_WORKGROUP = 16  // Increased from 8
  val BLOCK_SIZE = WARP_SIZE * WARPS_PER_WORKGROUP  // 512 threads

  case class Sizes(
    batchSize: Int,
    inFeatures: Int,      // C (hidden size, e.g. 2048)
    outFeatures: Int,     // FFN (intermediate size, e.g. 5632)
    gateOffsetVec4: Int,
    upOffsetVec4: Int,
    totalGateVec4: Int,
    totalUpVec4: Int,
  ):
    require(inFeatures % 4 == 0, s"inFeatures ($inFeatures) must be divisible by 4")
    require(inFeatures <= BLOCK_SIZE * 4, s"inFeatures ($inFeatures) must be <= ${BLOCK_SIZE * 4} for shared memory")
    def inFeaturesDiv4: Int = inFeatures / 4
    def totalOutputs: Int = batchSize * outFeatures
    def numWorkgroups: Int = (totalOutputs + WARPS_PER_WORKGROUP - 1) / WARPS_PER_WORKGROUP
    def numVecIterations: Int = (inFeaturesDiv4 + WARP_SIZE - 1) / WARP_SIZE
    // How many vec4s each thread loads to shared (inFeatures/4 / BLOCK_SIZE, rounded up)
    def loadsPerThread: Int = (inFeaturesDiv4 + BLOCK_SIZE - 1) / BLOCK_SIZE

  case class ProgramLayout(
    wgate: GBuffer[Vec4[Float16]],
    wup: GBuffer[Vec4[Float16]],
    input: GBuffer[Float16],  // Scalar input, loaded as Vec4 via reinterpret
    output: GBuffer[Float16],
  ) derives Layout

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val inFeatures = sizes.inFeatures
    val inFeaturesDiv4 = sizes.inFeaturesDiv4
    val outFeatures = sizes.outFeatures
    val gateOffsetVec4 = sizes.gateOffsetVec4
    val upOffsetVec4 = sizes.upOffsetVec4
    val numVecIterations = sizes.numVecIterations
    val loadsPerThread = sizes.loadsPerThread

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
      val tid = GIO.localInvocationIndex
      val workgroupId = GIO.workgroupId.x
      val laneId = tid.mod(WARP_SIZE)
      val warpId = tid / WARP_SIZE
      
      // Shared memory for input (loaded once, reused for both gate and up)
      // Using Float32 to avoid F16→F32 conversions in the hot loop
      val sharedInput = GShared[Vec4[Float32]](inFeaturesDiv4)
      
      val inFeaturesVal: Int32 = inFeatures
      val inFeaturesDiv4Val: Int32 = inFeaturesDiv4
      val outFeaturesVal: Int32 = outFeatures
      val gateOffsetVec4Val: Int32 = gateOffsetVec4
      val upOffsetVec4Val: Int32 = upOffsetVec4
      val totalOutputsVal: Int32 = sizes.totalOutputs
      val loadsPerThreadVal: Int32 = loadsPerThread
      val blockSizeVal: Int32 = BLOCK_SIZE

      // Output index for this warp
      val outputIdx = workgroupId * WARPS_PER_WORKGROUP + warpId
      
      // For batch > 1, compute which batch this output belongs to
      val batch = outputIdx / outFeaturesVal
      val outIdx = outputIdx.mod(outFeaturesVal)
      
      for
        // Phase 1: Cooperatively load input to shared memory
        // Each thread loads loadsPerThread vec4 elements (reading 4 scalars each)
        _ <- GIO.repeat(loadsPerThread): loadIdx =>
          val sharedIdx = tid + loadIdx * blockSizeVal
          GIO.when(sharedIdx < inFeaturesDiv4Val):
            val inputBase = batch * inFeaturesVal + sharedIdx * 4
            val x0 = GIO.read[Float16](layout.input, inputBase).asFloat32
            val x1 = GIO.read[Float16](layout.input, inputBase + 1).asFloat32
            val x2 = GIO.read[Float16](layout.input, inputBase + 2).asFloat32
            val x3 = GIO.read[Float16](layout.input, inputBase + 3).asFloat32
            sharedInput.write(sharedIdx, vec4(x0, x1, x2, x3))
        
        _ <- GIO.barrier
        
        // Phase 2: Compute gate and up dot products in SINGLE LOOP
        // Read input once per iteration, compute both gate and up dot products
        _ <- GIO.when(outputIdx < totalOutputsVal):
          // Use Vec2 to accumulate both gate and up sums in one loop
          val sums = GSeq
            .gen[Int32](laneId, _ + WARP_SIZE)
            .limit(numVecIterations)
            .unroll
            .fold(vec2(0.0f, 0.0f), (acc: Vec2[Float32], k: Int32) =>
              when(k < inFeaturesDiv4Val):
                // Read input ONCE
                val inputVec = sharedInput.read(k)
                
                // Read both weight vectors
                val gVec = GIO.read[Vec4[Float16]](layout.wgate, gateOffsetVec4Val + outIdx * inFeaturesDiv4Val + k)
                val uVec = GIO.read[Vec4[Float16]](layout.wup, upOffsetVec4Val + outIdx * inFeaturesDiv4Val + k)
                
                // Compute both dot products
                val gDot = gVec.x.asFloat32 * inputVec.x + gVec.y.asFloat32 * inputVec.y + 
                           gVec.z.asFloat32 * inputVec.z + gVec.w.asFloat32 * inputVec.w
                val uDot = uVec.x.asFloat32 * inputVec.x + uVec.y.asFloat32 * inputVec.y + 
                           uVec.z.asFloat32 * inputVec.z + uVec.w.asFloat32 * inputVec.w
                
                vec2(acc.x + gDot, acc.y + uDot)
              .otherwise(acc)
            )
          
          // Reduce both sums across warp
          val gate = GIO.subgroupAdd(sums.x)
          val up = GIO.subgroupAdd(sums.y)
          
          // SwiGLU: silu(gate) * up = gate * sigmoid(gate) * up
          val sigmoidGate = 1.0f / (1.0f + exp(-gate))
          val result = gate * sigmoidGate * up
          
          GIO.write[Float16](layout.output, outputIdx, result.asFloat16)
      yield GStruct.Empty()
