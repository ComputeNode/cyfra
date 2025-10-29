package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}
import io.computenode.cyfra.fluids.solver.*
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Comprehensive test of fluid solver programs */
object ComprehensiveFluidTest:
  
  def createParamsBuffer(dt: Float, viscosity: Float, diffusion: Float, 
                         buoyancy: Float, ambient: Float, gridSize: Int, 
                         iterationCount: Int): java.nio.ByteBuffer =
    val buffer = BufferUtils.createByteBuffer(32)
    buffer.putFloat(dt)
    buffer.putFloat(viscosity)
    buffer.putFloat(diffusion)
    buffer.putFloat(buoyancy)
    buffer.putFloat(ambient)
    buffer.putInt(gridSize)
    buffer.putInt(iterationCount)
    buffer.flip()
    buffer

  @main
  def testComprehensiveFluid(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 70)
      println("CYFRA FLUIDS - COMPREHENSIVE SIMULATION TEST")
      println("=" * 70)
      println()
      
      val gridSize = 8
      val totalCells = gridSize * gridSize * gridSize
      
      // Use regular Scala types for test parameters
      val dt = 0.1f
      val viscosity = 0.0001f
      val diffusion = 0.00001f
      val buoyancy = 1.0f
      val ambient = 0.0f
      val iterationCount = 2
      
      println(s"Grid: ${gridSize}³ = $totalCells cells")
      println(s"Time step: ${dt}s")
      println(s"Buoyancy: ${buoyancy}")
      println()
      
      // Test 1: Forces (Buoyancy)
      println("TEST 1: Forces Program (Buoyancy)")
      println("-" * 70)
      
      val forcesProgram = ForcesProgram.create
      val forcesRegion = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          forcesProgram.execute(totalCells, layout)
      
      // Initialize: zero velocity, hot spot in center
      val velocityData = Array.fill(totalCells * 3)(0.0f)
      val temperatureData = Array.ofDim[Float](totalCells)
      
      // Create hot sphere in center
      var hotCellCount = 0
      for i <- 0 until totalCells do
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        val centerX = gridSize / 2.0
        val centerY = gridSize / 2.0
        val centerZ = gridSize / 2.0
        val dist = Math.sqrt(
          Math.pow(x - centerX, 2) + 
          Math.pow(y - centerY, 2) + 
          Math.pow(z - centerZ, 2)
        )
        if dist < 1.5 then
          temperatureData(i) = 1.0f
          hotCellCount += 1
        else
          temperatureData(i) = 0.0f
      
      println(s"  Hot cells created: $hotCellCount")
      println(s"  Temperature range: [0.0, 1.0]")
      println(s"  Expected upward velocity: ~${buoyancy * (1.0f - ambient) * dt}")
      
      val velBuffer = BufferUtils.createByteBuffer(velocityData.length * 4)
      velBuffer.asFloatBuffer().put(velocityData).flip()
      
      val tempBuffer = BufferUtils.createByteBuffer(temperatureData.length * 4)
      tempBuffer.asFloatBuffer().put(temperatureData).flip()
      
      val paramsBuffer = createParamsBuffer(dt, viscosity, diffusion, buoyancy, ambient, gridSize, iterationCount)
      
      val velResultBuffer = BufferUtils.createFloatBuffer(totalCells * 3)
      val velResultBB = MemoryUtil.memByteBuffer(velResultBuffer)
      
      forcesRegion.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec3[Float32]](velBuffer),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](tempBuffer),
          divergence = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.velocity.read(velResultBB)
      )
      
      // Analyze results
      var hotCellsWithUpVel = 0
      var maxUpVel = 0.0f
      var avgUpVel = 0.0f
      var hotCellsChecked = 0
      
      for i <- 0 until totalCells do
        if temperatureData(i) > 0.5f then
          val vy = velResultBuffer.get(i * 3 + 1)  // Y component
          avgUpVel += vy
          hotCellsChecked += 1
          if vy > 0.001f then
            hotCellsWithUpVel += 1
          if vy > maxUpVel then
            maxUpVel = vy
      
      if hotCellsChecked > 0 then
        avgUpVel /= hotCellsChecked
      
      println(s"  Hot cells checked: $hotCellsChecked")
      println(s"  Hot cells with upward velocity (>0.001): $hotCellsWithUpVel")
      println(s"  Max upward velocity: $maxUpVel")
      println(s"  Average upward velocity in hot cells: $avgUpVel")
      
      val forcesPass = hotCellsWithUpVel > 0 && maxUpVel > 0.05f
      if forcesPass then
        println(s"  ✅ PASS: Buoyancy force correctly applied!")
      else
        println(s"  ❌ FAIL: No significant upward velocity detected")
        // Debug: Check a few cells
        println(s"\n  Debug: First 5 hot cells:")
        var debugCount = 0
        for i <- 0 until totalCells if debugCount < 5 && temperatureData(i) > 0.5f do
          val vx = velResultBuffer.get(i * 3 + 0)
          val vy = velResultBuffer.get(i * 3 + 1)
          val vz = velResultBuffer.get(i * 3 + 2)
          val temp = temperatureData(i)
          println(s"    Cell $i: temp=$temp, vel=($vx, $vy, $vz)")
          debugCount += 1
      
      println()
      
      // Test 2: Divergence Computation
      println("TEST 2: Projection Program (Divergence)")
      println("-" * 70)
      
      val divergenceProgram = ProjectionProgram.divergence
      val divRegion = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          divergenceProgram.execute(totalCells, layout)
      
      // Create expanding velocity field
      val divVelData = Array.ofDim[Float](totalCells * 3)
      for i <- 0 until totalCells do
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        val cx = gridSize / 2.0f
        val cy = gridSize / 2.0f
        val cz = gridSize / 2.0f
        // Velocity pointing away from center (positive divergence)
        divVelData(i * 3 + 0) = (x - cx) * 0.2f
        divVelData(i * 3 + 1) = (y - cy) * 0.2f
        divVelData(i * 3 + 2) = (z - cz) * 0.2f
      
      val divVelBuffer = BufferUtils.createByteBuffer(divVelData.length * 4)
      divVelBuffer.asFloatBuffer().put(divVelData).flip()
      
      val divResultBuffer = BufferUtils.createFloatBuffer(totalCells)
      val divResultBB = MemoryUtil.memByteBuffer(divResultBuffer)
      
      val paramsBuffer2 = createParamsBuffer(dt, viscosity, diffusion, buoyancy, ambient, gridSize, iterationCount)
      
      divRegion.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec3[Float32]](divVelBuffer),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](totalCells),
          divergence = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer2)
        ),
        onDone = layout => layout.divergence.read(divResultBB)
      )
      
      // Analyze divergence
      val divergences = (0 until totalCells).map(i => divResultBuffer.get(i))
      val nonZeroDiv = divergences.count(d => Math.abs(d) > 0.001f)
      val posDiv = divergences.count(_ > 0.001f)
      val negDiv = divergences.count(_ < -0.001f)
      val avgDiv = divergences.sum / totalCells
      val maxDiv = divergences.max
      val minDiv = divergences.min
      
      println(s"  Non-zero divergence cells: $nonZeroDiv / $totalCells")
      println(s"  Positive divergence cells: $posDiv")
      println(s"  Negative divergence cells: $negDiv")
      println(s"  Average divergence: $avgDiv")
      println(s"  Divergence range: [$minDiv, $maxDiv]")
      
      // Expected: expanding field should have positive divergence
      val divPass = nonZeroDiv > totalCells / 4 && posDiv > nonZeroDiv / 2
      if divPass then
        println(s"  ✅ PASS: Divergence computation works correctly!")
      else
        println(s"  ❌ FAIL: Divergence computation incorrect")
        // Debug: Check center cells
        println(s"\n  Debug: Center region divergences:")
        for z <- (gridSize/2 - 1) to (gridSize/2 + 1)
            y <- (gridSize/2 - 1) to (gridSize/2 + 1)
            x <- (gridSize/2 - 1) to (gridSize/2 + 1)
        do
          val idx = x + y * gridSize + z * gridSize * gridSize
          if idx < totalCells then
            val div = divResultBuffer.get(idx)
            println(s"    [$x,$y,$z]: divergence=$div")
      
      println()
      
      // Summary
      println("=" * 70)
      println("FINAL RESULTS")
      println("=" * 70)
      if forcesPass && divPass then
        println("✅ ALL TESTS PASSED!")
        println()
        println("Verified:")
        println("  ✅ Buoyancy force application (ForcesProgram)")
        println("  ✅ Divergence computation (ProjectionProgram)")
        println("  ✅ GPU memory operations")
        println("  ✅ Struct field access")
        println("  ✅ Int32 arithmetic")
        println("  ✅ 3D grid operations")
        println()
        println("🎉 Fluid simulation programs are working correctly on GPU!")
      else
        println("⚠️  SOME TESTS FAILED")
        println()
        if !forcesPass then
          println("  ❌ ForcesProgram needs investigation")
        if !divPass then
          println("  ❌ ProjectionProgram divergence needs investigation")
        println()
        println("Note: Programs compile and run, but physics calculations")
        println("may need validation or test data adjustment.")
      println("=" * 70)
      
    finally
      runtime.close()

