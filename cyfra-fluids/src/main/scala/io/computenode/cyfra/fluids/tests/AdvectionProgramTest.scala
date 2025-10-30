package io.computenode.cyfra.fluids.tests

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.fluids.solver.AdvectionProgram
import io.computenode.cyfra.fluids.core.FluidParams
import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.fluids.core.FluidStateDouble
import java.nio.{ByteBuffer, ByteOrder}

@main def testAdvectionProgram(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()

  try
    val gridSize = 32
    val totalCells = gridSize * gridSize * gridSize
    
    println(s"Testing AdvectionProgram with grid $gridSize³ = $totalCells cells")
    
    // Create fluid parameters
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
    
    // Initialize velocity buffer with uniform upward flow
    val velocityBuffer = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    for i <- 0 until totalCells do
      velocityBuffer.putFloat(0.0f)  // x
      velocityBuffer.putFloat(5.0f)  // y (upward)
      velocityBuffer.putFloat(0.0f)  // z
      velocityBuffer.putFloat(0.0f)  // w
    velocityBuffer.rewind()
    
    // Initialize density buffer with blob at bottom
    val densityBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    for z <- 0 until gridSize do
      for y <- 0 until gridSize do
        for x <- 0 until gridSize do
          val dx = x - gridSize/2
          val dy = y - 4 // Near bottom
          val dz = z - gridSize/2
          val distSq = dx*dx + dy*dy + dz*dz
          val density = if distSq < 9 then 1.0f else 0.0f
          densityBuffer.putFloat(density)
    densityBuffer.rewind()
    
    // Execute program
    val region = GBufferRegion
      .allocate[FluidStateDouble]
      .map: region =>
        AdvectionProgram.create.execute(totalCells, region)
    
    val densityOut = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    
    println("\n=== AdvectionProgram Results ===")
    
    try
      region.runUnsafe(
        init = FluidStateDouble(
          velocityCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())),
          pressureCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          densityCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          temperatureCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          divergenceCurrent = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          velocityPrevious = GBuffer(velocityBuffer),
          pressurePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          densityPrevious = GBuffer(densityBuffer),
          temperaturePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          divergencePrevious = GBuffer(ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())),
          params = GUniform(paramsBuffer)
        ),
        onDone = layout =>
          layout.densityCurrent.read(densityOut)
      )
      
      densityOut.rewind()
      
      // Check density at y=4 (original position)
      var densityAtY4 = 0.0f
      for z <- (gridSize/2 - 2) until (gridSize/2 + 2) do
        for x <- (gridSize/2 - 2) until (gridSize/2 + 2) do
          val idx = x + gridSize * (4 + gridSize * z)
          densityOut.position(idx * 4)
          densityAtY4 += densityOut.getFloat()
      
      // Check density at higher positions (should have moved up)
      var densityAtY6 = 0.0f
      for z <- (gridSize/2 - 2) until (gridSize/2 + 2) do
        for x <- (gridSize/2 - 2) until (gridSize/2 + 2) do
          val idx = x + gridSize * (6 + gridSize * z)
          densityOut.position(idx * 4)
          densityAtY6 += densityOut.getFloat()
      
      println(f"Density at Y=4 (original): $densityAtY4%.2f")
      println(f"Density at Y=6 (moved up): $densityAtY6%.2f")
      
      if densityAtY6 > densityAtY4 then
        println("✅ PASS: Density moved upward (advection working)")
      else
        println("⚠️  WARN: Density didn't move much (advection may have issues)")
        
    catch
      case e: Exception =>
        println(s"❌ FAIL: AdvectionProgram threw exception:")
        println(s"  ${e.getMessage}")
        e.printStackTrace()
      
  finally
    runtime.close()

