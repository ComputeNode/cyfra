package io.computenode.cyfra.fluids.tests

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.fluids.solver.ForcesProgram
import io.computenode.cyfra.fluids.core.FluidParams
import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.fluids.core.FluidState
import java.nio.{ByteBuffer, ByteOrder}

@main def testForcesProgram(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()

  try
    val gridSize = 32
    val totalCells = gridSize * gridSize * gridSize
    
    println(s"Testing ForcesProgram with grid $gridSize³ = $totalCells cells")
    
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
    
    // Check results
    velocityOut.rewind()
    println("\n=== ForcesProgram Results ===")
    println("First 10 cells (after buoyancy):")
    for i <- 0 until 10 do
      val vx = velocityOut.getFloat()
      val vy = velocityOut.getFloat()
      val vz = velocityOut.getFloat()
      val vw = velocityOut.getFloat()
      println(f"Cell $i: vel=($vx%.4f, $vy%.4f, $vz%.4f, $vw%.4f)")
    
    // Find max Y velocity
    velocityOut.rewind()
    var maxVy = 0.0f
    for i <- 0 until totalCells do
      velocityOut.position(i * 16 + 4) // Skip to Y component
      val vy = velocityOut.getFloat()
      if math.abs(vy) > math.abs(maxVy) then maxVy = vy
    
    println(f"\nMax Y velocity: $maxVy%.4f")
    println(f"Expected: ~${8.0f * 0.3f * 10.0f}%.4f (buoyancy * dt * temp)")
    
    if maxVy > 1.0f then
      println("✅ PASS: Buoyancy force applied correctly")
    else
      println("❌ FAIL: Buoyancy force too weak or not applied")
      
  finally
    runtime.close()
