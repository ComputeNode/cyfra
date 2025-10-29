package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Test: Just read velocity at a specific index and write components separately */
object VelocityReadTest:
  
  val readProgram = GProgram[Int, FluidState](
    layout = totalCells => {
      import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
      FluidState(
        velocity = GBuffer[Vec3[Float32]](totalCells),
        pressure = GBuffer[Float32](totalCells),
        density = GBuffer[Float32](totalCells),
        temperature = GBuffer[Float32](totalCells),
        divergence = GBuffer[Float32](totalCells),
        params = GUniform[FluidParams]()
      )
    },
    dispatch = (_, totalCells) => StaticDispatch((1, 1, 1)),
    workgroupSize = (10, 1, 1)
  ): state =>
    val idx = GIO.invocationId
    val vel = GIO.read(state.velocity, idx)
    
    // Write x, y, z components to pressure, density, temperature for inspection
    for
      _ <- GIO.write(state.pressure, idx, vel.x)
      _ <- GIO.write(state.density, idx, vel.y)
      _ <- GIO.write(state.temperature, idx, vel.z)
      _ <- GIO.write(state.divergence, idx, idx.asFloat)  // Just write idx for verification
    yield Empty()
  
  @main
  def testVelocityRead(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 70)
      println("Velocity Read Test - Verify Vec3 Component Access")
      println("=" * 70)
      println()
      
      val totalCells = 10
      
      // Create test velocity data
      val velData = Array.ofDim[Float](totalCells * 3)
      for i <- 0 until totalCells do
        velData(i * 3 + 0) = i * 1.0f      // x = i
        velData(i * 3 + 1) = i * 10.0f     // y = i * 10
        velData(i * 3 + 2) = i * 100.0f    // z = i * 100
      
      println("Input velocity data:")
      for i <- 0 until totalCells do
        println(s"  vel[$i] = (${velData(i * 3 + 0)}, ${velData(i * 3 + 1)}, ${velData(i * 3 + 2)})")
      println()
      
      val region = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          readProgram.execute(totalCells, layout)
      
      val velBuffer = BufferUtils.createByteBuffer(velData.length * 4)
      velBuffer.asFloatBuffer().put(velData).flip()
      
      val paramsBuffer = BufferUtils.createByteBuffer(32)
      paramsBuffer.putFloat(0.1f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putFloat(1.0f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putInt(8)
      paramsBuffer.putInt(1)
      paramsBuffer.flip()
      
      val pressureBuffer = BufferUtils.createFloatBuffer(totalCells)
      val pressureBB = MemoryUtil.memByteBuffer(pressureBuffer)
      
      val densityBuffer = BufferUtils.createFloatBuffer(totalCells)
      val densityBB = MemoryUtil.memByteBuffer(densityBuffer)
      
      val temperatureBuffer = BufferUtils.createFloatBuffer(totalCells)
      val temperatureBB = MemoryUtil.memByteBuffer(temperatureBuffer)
      
      val divBuffer = BufferUtils.createFloatBuffer(totalCells)
      val divBB = MemoryUtil.memByteBuffer(divBuffer)
      
      region.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec3[Float32]](velBuffer),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](totalCells),
          divergence = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => {
          layout.pressure.read(pressureBB)
          layout.density.read(densityBB)
          layout.temperature.read(temperatureBB)
          layout.divergence.read(divBB)
        }
      )
      
      println("GPU Output (components extracted):")
      var allCorrect = true
      for i <- 0 until totalCells do
        val expectedX = i * 1.0f
        val expectedY = i * 10.0f
        val expectedZ = i * 100.0f
        
        val actualX = pressureBuffer.get(i)
        val actualY = densityBuffer.get(i)
        val actualZ = temperatureBuffer.get(i)
        val actualIdx = divBuffer.get(i)
        
        val correct = 
          Math.abs(actualX - expectedX) < 0.001f &&
          Math.abs(actualY - expectedY) < 0.001f &&
          Math.abs(actualZ - expectedZ) < 0.001f &&
          Math.abs(actualIdx - i) < 0.001f
        
        if !correct then allCorrect = false
        
        val status = if correct then "✓" else "✗"
        println(s"  $status vel[$i] GPU: x=$actualX (expect $expectedX), y=$actualY (expect $expectedY), z=$actualZ (expect $expectedZ), idx=$actualIdx")
      
      println()
      if allCorrect then
        println("✅ ALL CORRECT! Vec3 reading works perfectly.")
      else
        println("❌ ERRORS FOUND in Vec3 reading.")
      
      println("=" * 70)
      
    finally
      runtime.close()

