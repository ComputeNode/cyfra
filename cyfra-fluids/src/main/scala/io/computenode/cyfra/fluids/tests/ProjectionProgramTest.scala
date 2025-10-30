package io.computenode.cyfra.fluids.tests

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.fluids.solver.ProjectionProgram
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState, FluidStateDouble}
import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import java.nio.{ByteBuffer, ByteOrder}

@main def testProjectionProgram(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()

  try
    val gridSize = 16
    val totalCells = gridSize * gridSize * gridSize
    
    println(s"Testing ProjectionProgram with grid $gridSize³ = $totalCells cells")
    
    val params = FluidParams(
      dt = 0.3f,
      viscosity = 0.0001f,
      diffusion = 0.0001f,
      buoyancy = 8.0f,
      ambient = 0.0f,
      gridSize = gridSize,
      iterationCount = 20,
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
    
    println("\n=== Testing Step 1: Divergence Computation ===")
    
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
    
    try
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
      val centerIdx = gridSize/2 + gridSize * (gridSize/2 + gridSize * gridSize/2)
      divergenceOut.position(centerIdx * 4)
      val centerDiv = divergenceOut.getFloat()
      
      println(f"Divergence at center: $centerDiv%.4f")
      println(f"Expected: ~0.3 (positive = expanding)")
      
      if centerDiv > 0.1f then
        println("✅ PASS: Divergence computed correctly")
      else
        println("⚠️  WARN: Divergence may be incorrect")
      
      println("\n=== Testing Step 2: Pressure Solve ===")
      
      // Initialize divergence buffer with constant positive divergence
      val divBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
      for i <- 0 until totalCells do
        divBuffer.putFloat(1.0f)
      divBuffer.rewind()
      
      val pressureSolveProgram = ProjectionProgram.pressureSolve
      
      val region2 = GBufferRegion
        .allocate[FluidStateDouble]
        .map: region =>
          pressureSolveProgram.execute(totalCells, region)
      
      val pressureOut = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
      
      region2.runUnsafe(
        init = FluidStateDouble(
          velocityCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())),
          pressureCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          densityCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          temperatureCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          divergenceCurrent = GBuffer(divBuffer),
          velocityPrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())),
          pressurePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          densityPrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          temperaturePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          divergencePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          params = GUniform(paramsBuffer)
        ),
        onDone = layout =>
          layout.pressureCurrent.read(pressureOut)
      )
      
      pressureOut.rewind()
      pressureOut.position(centerIdx * 4)
      val centerPressure = pressureOut.getFloat()
      
      println(f"Pressure at center: $centerPressure%.4f")
      println("Expected: negative value (one Jacobi iteration)")
      
      if centerPressure < 0.0f then
        println("✅ PASS: Pressure solve working")
      else
        println("⚠️  WARN: Pressure solve may be incorrect")
      
      println("\n=== Testing Step 3: Subtract Gradient ===")
      
      // Initialize velocity and pressure for gradient subtraction
      velocityBuffer.rewind()
      val pressureBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
      for z <- 0 until gridSize do
        for y <- 0 until gridSize do
          for x <- 0 until gridSize do
            // Pressure increases with Y
            pressureBuffer.putFloat(y.toFloat)
      pressureBuffer.rewind()
      
      val subtractGradientProgram = ProjectionProgram.subtractGradient
      
      val region3 = GBufferRegion
        .allocate[FluidState]
        .map: region =>
          subtractGradientProgram.execute(totalCells, region)
      
      val velocityOut = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
      
      region3.runUnsafe(
        init = FluidState(
          velocity = GBuffer(velocityBuffer),
          pressure = GBuffer(pressureBuffer),
          density = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          temperature = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          divergence = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          params = GUniform(paramsBuffer)
        ),
        onDone = layout =>
          layout.velocity.read(velocityOut)
      )
      
      velocityOut.rewind()
      velocityOut.position(centerIdx * 16 + 4) // Y component
      val centerVy = velocityOut.getFloat()
      
      println(f"Center Y velocity after gradient subtraction: $centerVy%.4f")
      println("Expected: decreased from original due to pressure gradient")
      
      // Original centerVy was ~0.0, after subtracting positive gradient should be negative
      if centerVy < 0.0f then
        println("✅ PASS: Gradient subtraction working")
      else
        println("⚠️  WARN: Gradient subtraction may be incorrect")
        
    catch
      case e: Exception =>
        println(s"❌ FAIL: ProjectionProgram threw exception:")
        println(s"  ${e.getMessage}")
        e.printStackTrace()
      
  finally
    runtime.close()

