package io.computenode.cyfra.fluids.solver


import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.*
import io.computenode.cyfra.runtime.VkCyfraRuntime

import java.nio.{ByteBuffer, ByteOrder}

class DiffusionProgramTest extends munit.FunSuite:
  
  var runtime: VkCyfraRuntime = null
  
  override def beforeAll(): Unit =
    runtime = VkCyfraRuntime()
  
  override def afterAll(): Unit =
    if runtime != null then runtime.close()
  
  test("DiffusionProgram smooths velocity field"):
    val gridSize = 16  // Smaller grid for faster testing
    val totalCells = gridSize * gridSize * gridSize
    
    // Create fluid parameters with high viscosity for visible effect
    val params = FluidParams(
      dt = 0.3f,
      viscosity = 0.5f,  // High viscosity for visible diffusion
      diffusion = 0.1f,
      buoyancy = 8.0f,
      ambient = 0.0f,
      gridSize = gridSize,
      windX = 0.0f,
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
    
    // Initialize velocity buffer with sharp spike at center
    val velocityBuffer = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val isCentral = (x == gridSize/2 && y == gridSize/2 && z == gridSize/2)
          val vy = if isCentral then 100.0f else 0.0f
          velocityBuffer.putFloat(0.0f)  // x
          velocityBuffer.putFloat(vy)    // y (sharp spike)
          velocityBuffer.putFloat(0.0f)  // z
          velocityBuffer.putFloat(0.0f)  // w
    velocityBuffer.rewind()
    
    // Execute program
    given VkCyfraRuntime = runtime
    
    val region = GBufferRegion
      .allocate[FluidStateDouble]
      .map: region =>
        DiffusionProgram.create.execute(totalCells, region)
    
    val velocityOut = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    
    region.runUnsafe(
      init = FluidStateDouble(
        velocityCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())),
        pressureCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        densityCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        temperatureCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        divergenceCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        velocityPrevious = GBuffer(velocityBuffer),
        pressurePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        densityPrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        temperaturePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        divergencePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        params = GUniform(paramsBuffer)
      ),
      onDone = layout =>
        layout.velocityCurrent.read(velocityOut)
    )
    
    velocityOut.rewind()
    
    // Check central cell
    val centerIdx = (gridSize/2) + gridSize * ((gridSize/2) + gridSize * (gridSize/2))
    velocityOut.position(centerIdx * 16 + 4)  // Y component
    val centerVy = velocityOut.getFloat()
    
    // Check neighbor cell (should have been diffused from center)
    val neighborIdx = (gridSize/2 + 1) + gridSize * ((gridSize/2) + gridSize * (gridSize/2))
    velocityOut.position(neighborIdx * 16 + 4)  // Y component
    val neighborVy = velocityOut.getFloat()
    
    // Assert diffusion occurred
    assert(centerVy < 100.0f, s"Center velocity ($centerVy) should be less than initial (100.0) after diffusion")
    assert(neighborVy > 0.0f, s"Neighbor velocity ($neighborVy) should be > 0 after diffusion")
    assert(centerVy > neighborVy, s"Center ($centerVy) should still be larger than neighbor ($neighborVy)")
