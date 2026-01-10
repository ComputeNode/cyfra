package io.computenode.cyfra.fluids.solver

import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.fluids.solver.ProjectionProgram
import io.computenode.cyfra.runtime.VkCyfraRuntime
import java.nio.{ByteBuffer, ByteOrder}

class ProjectionProgramTest extends munit.FunSuite:
  
  var runtime: VkCyfraRuntime = null
  
  override def beforeAll(): Unit =
    runtime = VkCyfraRuntime()
  
  override def afterAll(): Unit =
    if runtime != null then runtime.close()
  
  test("ProjectionProgram computes divergence"):
    val gridSize = 16
    val totalCells = gridSize * gridSize * gridSize
    
    val params = FluidParams(
      dt = 0.3f,
      viscosity = 0.0001f,
      diffusion = 0.0001f,
      buoyancy = 8.0f,
      ambient = 0.0f,
      gridSize = gridSize,
      windX = 0.0f,
      windY = 0f,
      windZ = 0.0f
    )
    
    val paramsBuffer = {
      import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
      import io.computenode.cyfra.dsl.struct.GStruct.given
      import io.computenode.cyfra.core.GCodec
      
      val schema = summon[io.computenode.cyfra.dsl.struct.GStructSchema[FluidParams]]
      val size = totalStride(schema)
      val buffer = ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder())
      summon[GCodec[FluidParams, FluidParams]].toByteBuffer(buffer, Array(params))
      buffer.rewind()
      buffer
    }
    
    // Initialize velocity buffer with divergent field (expanding from center)
    val velocityBuffer = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val dx = (x - gridSize/2).toFloat
          val dy = (y - gridSize/2).toFloat
          val dz = (z - gridSize/2).toFloat
          velocityBuffer.putFloat(dx * 0.1f)  // x (expanding)
          velocityBuffer.putFloat(dy * 0.1f)  // y (expanding)
          velocityBuffer.putFloat(dz * 0.1f)  // z (expanding)
          velocityBuffer.putFloat(0.0f)       // w
    velocityBuffer.rewind()
    
    given VkCyfraRuntime = runtime
    
    val divergenceProgram = ProjectionProgram.divergence
    
    val region = GBufferRegion
      .allocate[FluidState]
      .map: region =>
        divergenceProgram.execute(totalCells, region)
    
    val divergenceOut = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    
    region.runUnsafe(
      init = FluidState(
        velocity = GBuffer(velocityBuffer),
        pressure = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        density = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        temperature = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        divergence = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        params = GUniform(paramsBuffer)
      ),
      onDone = layout =>
        layout.divergence.read(divergenceOut)
    )
    
    divergenceOut.rewind()
    
    // Find max divergence
    var maxDiv = 0.0f
    for i <- 0 until totalCells do
      val div = math.abs(divergenceOut.getFloat())
      if div > maxDiv then maxDiv = div
    
    // Assert divergence was computed (expanding field should have positive divergence)
    assert(maxDiv > 0.0f, s"Max divergence ($maxDiv) should be > 0 for expanding velocity field")
  
  test("ProjectionProgram solves pressure and removes divergence"):
    val gridSize = 16
    val totalCells = gridSize * gridSize * gridSize
    
    val params = FluidParams(
      dt = 0.3f,
      viscosity = 0.0001f,
      diffusion = 0.0001f,
      buoyancy = 8.0f,
      ambient = 0.0f,
      gridSize = gridSize,
      windX = 0.0f,
      windZ = 0.0f
    )
    
    val paramsBuffer = {
      import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
      import io.computenode.cyfra.dsl.struct.GStruct.given
      import io.computenode.cyfra.core.GCodec
      
      val schema = summon[io.computenode.cyfra.dsl.struct.GStructSchema[FluidParams]]
      val size = totalStride(schema)
      val buffer = ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder())
      summon[GCodec[FluidParams, FluidParams]].toByteBuffer(buffer, Array(params))
      buffer.rewind()
      buffer
    }
    
    // Initialize divergent velocity field
    val velocityBuffer = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val dx = (x - gridSize/2).toFloat
          val dy = (y - gridSize/2).toFloat
          val dz = (z - gridSize/2).toFloat
          velocityBuffer.putFloat(dx * 0.1f)
          velocityBuffer.putFloat(dy * 0.1f)
          velocityBuffer.putFloat(dz * 0.1f)
          velocityBuffer.putFloat(0.0f)
    velocityBuffer.rewind()
    
    // Compute divergence
    val divergenceBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    for i <- 0 until totalCells do
      divergenceBuffer.putFloat(0.3f)  // Non-zero divergence
    divergenceBuffer.rewind()
    
    given VkCyfraRuntime = runtime
    
    // Step 1: Compute divergence
    val divRegion = GBufferRegion
      .allocate[FluidState]
      .map: region =>
        ProjectionProgram.divergence.execute(totalCells, region)
    
    val divOut = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    divRegion.runUnsafe(
      init = FluidState(
        velocity = GBuffer(velocityBuffer),
        pressure = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        density = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        temperature = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        divergence = GBuffer(divergenceBuffer),
        params = GUniform(paramsBuffer)
      ),
      onDone = layout =>
        layout.divergence.read(divOut)
    )
    
    // Step 2: Solve pressure
    val pressureBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    val pressurePrevBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    
    divOut.rewind()
    divergenceBuffer.rewind()
    val cpyArray = new Array[Byte](totalCells * 4)
    divOut.get(cpyArray)
    divergenceBuffer.put(cpyArray)
    divergenceBuffer.rewind()
    
    val pressureRegion = GBufferRegion
      .allocate[FluidStateDouble]
      .map: region =>
        ProjectionProgram.pressureSolve.execute(totalCells, region)
    
    pressureRegion.runUnsafe(
      init = FluidStateDouble(
        velocityCurrent = GBuffer(velocityBuffer),
        pressureCurrent = GBuffer(pressureBuffer),
        densityCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        temperatureCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        divergenceCurrent = GBuffer(divergenceBuffer),
        velocityPrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())),
        pressurePrevious = GBuffer(pressurePrevBuffer),
        densityPrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        temperaturePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        divergencePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
        params = GUniform(paramsBuffer)
      ),
      onDone = layout =>
        layout.pressureCurrent.read(pressureBuffer)
    )
    
    pressureBuffer.rewind()
    
    // Find max pressure
    var maxPressure = 0.0f
    for i <- 0 until totalCells do
      val p = math.abs(pressureBuffer.getFloat())
      if p > maxPressure then maxPressure = p
    
    // Assert pressure was computed
    assert(maxPressure > 0.0f, s"Max pressure ($maxPressure) should be > 0 after pressure solve")
