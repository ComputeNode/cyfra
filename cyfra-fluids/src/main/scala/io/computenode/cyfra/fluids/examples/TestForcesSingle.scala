package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.fluids.core.{FluidParams, FluidState}
import io.computenode.cyfra.fluids.solver.ForcesProgram
import org.lwjgl.BufferUtils

import java.nio.{ByteBuffer, ByteOrder}

/** Test ForcesProgram in isolation */
@main def testForcesSingle(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()
  
  try
    println("=== Testing ForcesProgram Alone ===")
    
    val gridSize = 8
    val totalCells = gridSize * gridSize * gridSize
    
    // Create params
    val params = FluidParams(
      dt = 0.3f,
      viscosity = 0.0001f,
      diffusion = 0.001f,
      buoyancy = 4.0f,
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
    
    // Create buffers - set temperature to 3.0 at index 0
    val velocityIn = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    velocityIn.rewind()
    
    val temperatureIn = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    for i <- 0 until totalCells do
      temperatureIn.putFloat(if i < 10 then 3.0f else 0.0f)
    temperatureIn.rewind()
    
    val pressureBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    val densityBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    val divergenceBuffer = ByteBuffer.allocateDirect(totalCells * 4).order(ByteOrder.nativeOrder())
    
    // Execute ForcesProgram
    val region = GBufferRegion
      .allocate[FluidState]
      .map: region =>
        ForcesProgram.create.execute(totalCells, region)
    
    val velocityOut = ByteBuffer.allocateDirect(totalCells * 16).order(ByteOrder.nativeOrder())
    
    region.runUnsafe(
      init = FluidState(
        velocity = GBuffer(velocityIn),
        pressure = GBuffer(pressureBuffer),
        density = GBuffer(densityBuffer),
        temperature = GBuffer(temperatureIn),
        divergence = GBuffer(divergenceBuffer),
        params = GUniform(paramsBuffer)
      ),
      onDone = layout =>
        layout.velocity.read(velocityOut)
    )
    
    // Check results
    velocityOut.rewind()
    println("\nResults (first 10 cells):")
    for i <- 0 until 10 do
      val vx = velocityOut.getFloat()
      val vy = velocityOut.getFloat()
      val vz = velocityOut.getFloat()
      val vw = velocityOut.getFloat()
      println(s"  Cell $i: vy = $vy (expected ~${8.0f * 3.0f * 0.3f} = 7.2)")
    
  finally
    runtime.close()

