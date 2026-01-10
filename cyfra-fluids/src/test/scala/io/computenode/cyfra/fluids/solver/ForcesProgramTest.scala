package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.{FluidParams, FluidState, ForcesProgram}
import io.computenode.cyfra.runtime.VkCyfraRuntime

import java.nio.{ByteBuffer, ByteOrder}

class ForcesProgramTest extends munit.FunSuite:
  
  var runtime: VkCyfraRuntime = null
  
  override def beforeAll(): Unit =
    runtime = VkCyfraRuntime()
  
  override def afterAll(): Unit =
    if runtime != null then runtime.close()
  
  test("ForcesProgram applies buoyancy force correctly"):
    val gridSize = 32
    val totalCells = gridSize * gridSize * gridSize
    
    // Create fluid parameters
    val params = FluidParams(
      dt = 0.3f,
      viscosity = 0.0000f,
      diffusion = 0.0000f,
      buoyancy = 8.0f,
      ambient = 0.0f,
      gridSize = gridSize,
      windX = 0.0f,
      windY = 0f,
      windZ = 0.0f
    )
    
    val paramsBuffer = {
      import io.computenode.cyfra.core.GCodec
      import io.computenode.cyfra.dsl.struct.GStruct.given
      import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
      
      val schema = summon[io.computenode.cyfra.dsl.struct.GStructSchema[FluidParams]]
      val size = totalStride(schema)
      val buffer = ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder())
      summon[GCodec[FluidParams, FluidParams]].toByteBuffer(buffer, Array(params))
      buffer.rewind()
      buffer
    }
    
    // Initialize velocity buffer (all zero)
    val velocityBuffer = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    velocityBuffer.rewind()
    
    // Initialize temperature buffer with hot spot in center
    val temperatureBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val dx = x - gridSize/2
          val dy = y - gridSize/2
          val dz = z - gridSize/2
          val distSq = dx*dx + dy*dy + dz*dz
          val temp = if distSq < 16 then 10.0f else 0.0f
          temperatureBuffer.putFloat(temp)
    temperatureBuffer.rewind()
    
    // Execute program
    given VkCyfraRuntime = runtime
    
    val region = GBufferRegion
      .allocate[FluidState]
      .map: region =>
        ForcesProgram.create.execute(totalCells, region)
    
    val velocityOut = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    
    region.runUnsafe(
      init = FluidState(
        velocity = GBuffer(velocityBuffer),
        pressure = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        density = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        temperature = GBuffer(temperatureBuffer),
        divergence = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        params = GUniform(paramsBuffer)
      ),
      onDone = layout =>
        layout.velocity.read(velocityOut)
    )
    
    // Find max Y velocity
    velocityOut.rewind()
    var maxVy = 0.0f
    for i <- 0 until totalCells do
      velocityOut.position(i * 16 + 4) // Skip to Y component
      val vy = velocityOut.getFloat()
      if math.abs(vy) > math.abs(maxVy) then maxVy = vy
    
    val expectedVy = 8.0f * 0.3f * 10.0f
    
    // Assert buoyancy force was applied
    assert(maxVy > 1.0f, s"Max Y velocity ($maxVy) should be > 1.0 (expected ~$expectedVy)")
    assert(maxVy > expectedVy * 0.9f, s"Max Y velocity ($maxVy) should be at least 90% of expected (~$expectedVy)")
