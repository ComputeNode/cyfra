package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState, FluidStateDouble}
import io.computenode.cyfra.fluids.solver.*
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Test the complete fluid solver pipeline */
object FluidSolverTest:
  
  @main
  def testFluidSolver(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 60)
      println("Cyfra Fluids - Solver Pipeline Test")
      println("=" * 60)
      println()
      
      val gridSize = 8
      val totalCells = gridSize * gridSize * gridSize
      
      println(s"Grid: ${gridSize}³ = $totalCells cells")
      println()
      
      // Create fluid parameters
      val params = FluidParams(
        dt = 0.1f,
        viscosity = 0.0001f,
        diffusion = 0.00001f,
        buoyancy = 1.0f,
        ambient = 0.0f,
        gridSize = gridSize,
        iterationCount = 2  // Just 2 iterations for testing
      )
      
      println("Testing ForcesProgram...")
      
      // Test 1: Forces Program
      val forcesProgram = ForcesProgram.create
      
      val forcesRegion = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          forcesProgram.execute(totalCells, layout)
      
      // Initialize with zero velocity (Vec4 with w=0) and some temperature
      val velocityData = Array.fill(totalCells * 4)(0.0f)
      val temperatureData = Array.ofDim[Float](totalCells)
      for i <- 0 until totalCells do
        // Put hot spot in center
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        val centerDist = Math.sqrt(Math.pow(x - gridSize/2.0, 2) + Math.pow(y - gridSize/2.0, 2) + Math.pow(z - gridSize/2.0, 2))
        temperatureData(i) = if centerDist < 2.0 then 1.0f else 0.0f
      
      val velBuffer = BufferUtils.createByteBuffer(velocityData.length * 4)
      velBuffer.asFloatBuffer().put(velocityData).flip()
      
      val tempBuffer = BufferUtils.createByteBuffer(temperatureData.length * 4)
      tempBuffer.asFloatBuffer().put(temperatureData).flip()
      
      val velResultBuffer = BufferUtils.createFloatBuffer(totalCells * 4)
      val velResultBB = MemoryUtil.memByteBuffer(velResultBuffer)
      
      // Create params buffer with regular Scala values
      val paramsBuffer = BufferUtils.createByteBuffer(32) // Size for FluidParams struct
      paramsBuffer.putFloat(0.1f)  // dt
      paramsBuffer.putFloat(0.0001f)  // viscosity
      paramsBuffer.putFloat(0.00001f)  // diffusion
      paramsBuffer.putFloat(1.0f)  // buoyancy
      paramsBuffer.putFloat(0.0f)  // ambient
      paramsBuffer.putInt(gridSize)  // gridSize
      paramsBuffer.putInt(2)  // iterationCount
      paramsBuffer.flip()
      
      forcesRegion.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec4[Float32]](velBuffer),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](tempBuffer),
          divergence = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.velocity.read(velResultBB)
      )
      
      // Check that buoyancy force was applied
      var hotCellsWithUpwardVelocity = 0
      for i <- 0 until totalCells do
        val vy = velResultBuffer.get(i * 4 + 1)  // Y component
        if temperatureData(i) > 0.5f && vy > 0.0f then
          hotCellsWithUpwardVelocity += 1
      
      val hotCells = temperatureData.count(_ > 0.5f)
      println(s"  Hot cells: $hotCells")
      println(s"  Hot cells with upward velocity: $hotCellsWithUpwardVelocity")
      
      if hotCellsWithUpwardVelocity > 0 then
        println(s"  ✅ ForcesProgram: Buoyancy force applied correctly!")
      else
        println(s"  ❌ ForcesProgram: No upward velocity detected")
      println()
      
      // Test 2: Projection Program - Divergence
      println("Testing ProjectionProgram (divergence)...")
      
      val divRegion = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          ProjectionProgram.divergence.execute(totalCells, layout)
      
      // Create velocity field with divergence (expanding from center, Vec4 with w=0)
      val divVelData = Array.ofDim[Float](totalCells * 4)
      for i <- 0 until totalCells do
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        val dx = (x - gridSize/2.0f) * 0.1f
        val dy = (y - gridSize/2.0f) * 0.1f
        val dz = (z - gridSize/2.0f) * 0.1f
        divVelData(i * 4 + 0) = dx
        divVelData(i * 4 + 1) = dy
        divVelData(i * 4 + 2) = dz
        divVelData(i * 4 + 3) = 0.0f  // w component
      
      val divVelBuffer = BufferUtils.createByteBuffer(divVelData.length * 4)
      divVelBuffer.asFloatBuffer().put(divVelData).flip()
      
      val divResultBuffer = BufferUtils.createFloatBuffer(totalCells)
      val divResultBB = MemoryUtil.memByteBuffer(divResultBuffer)
      
      val paramsBuffer2 = BufferUtils.createByteBuffer(32) // Size for FluidParams struct
      paramsBuffer2.putFloat(0.1f)  // dt
      paramsBuffer2.putFloat(0.0001f)  // viscosity
      paramsBuffer2.putFloat(0.00001f)  // diffusion
      paramsBuffer2.putFloat(1.0f)  // buoyancy
      paramsBuffer2.putFloat(0.0f)  // ambient
      paramsBuffer2.putInt(gridSize)  // gridSize
      paramsBuffer2.putInt(2)  // iterationCount
      paramsBuffer2.flip()
      
      divRegion.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec4[Float32]](divVelBuffer),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](totalCells),
          divergence = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer2)
        ),
        onDone = layout => layout.divergence.read(divResultBB)
      )
      
      // Check divergence values
      val divergences = (0 until totalCells).map(i => divResultBuffer.get(i))
      val avgDiv = divergences.sum / totalCells
      val nonZeroDivs = divergences.count(Math.abs(_) > 0.001f)
      
      println(s"  Average divergence: $avgDiv")
      println(s"  Non-zero divergence cells: $nonZeroDivs / $totalCells")
      
      if nonZeroDivs > totalCells / 4 then
        println(s"  ✅ Divergence computation: Working correctly!")
      else
        println(s"  ❌ Divergence computation: Too few non-zero values")
      println()
      
      // Summary
      println("=" * 60)
      if hotCellsWithUpwardVelocity > 0 && nonZeroDivs > totalCells / 4 then
        println("✅ FLUID SOLVER PROGRAMS WORKING!")
        println()
        println("Verified operations:")
        println("  - Buoyancy force application (ForcesProgram)")
        println("  - Divergence computation (ProjectionProgram)")
        println("  - GBuffer read/write operations")
        println("  - GUniform parameter passing")
        println("  - 3D grid operations")
      else
        println("❌ SOME TESTS FAILED")
        println("Check the individual test results above")
      println("=" * 60)
      
    finally
      runtime.close()

