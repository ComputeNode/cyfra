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

/** Test just the index calculation and printf without any conditionals */
object IndexPrintTest:
  
  val indexProgram = GProgram[Int, FluidState](
    layout = totalCells => {
      import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
      FluidState(
        velocity = GBuffer[Vec4[Float32]](totalCells),
        pressure = GBuffer[Float32](totalCells),
        density = GBuffer[Float32](totalCells),
        temperature = GBuffer[Float32](totalCells),
        divergence = GBuffer[Float32](totalCells),
        params = GUniform[FluidParams]()
      )
    },
    dispatch = (_, totalCells) => StaticDispatch((1, 1, 1)),  // Just 1 workgroup
    workgroupSize = (10, 1, 1)  // 10 threads
  ): state =>
    val idx = GIO.invocationId
    val params = state.params.read
    val n = params.gridSize

    // Convert to 3D
    val z = idx / (n * n)
    val y = (idx / n).mod(n)
    val x = idx.mod(n)

    // Print unconditionally for first 10 threads
    for
      _ <- GIO.printf("Thread %d: x=%d, y=%d, z=%d\n", idx, x, y, z)
      _ <- GIO.write(state.divergence, idx, idx.asFloat)
    yield Empty()
  
  @main
  def testIndexPrint(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 60)
      println("Index Printf Test")
      println("=" * 60)
      println()
      println("Expecting output from first 10 threads:")
      println("-" * 60)
      
      val gridSize = 8
      val totalCells = 10  // Just test first 10
      
      val region = GBufferRegion
        .allocate[FluidState]
        .map: layout =>
          indexProgram.execute(totalCells, layout)
      
      val paramsBuffer = BufferUtils.createByteBuffer(32)
      paramsBuffer.putFloat(0.1f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putFloat(1.0f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putInt(gridSize)
      paramsBuffer.putInt(1)
      paramsBuffer.flip()
      
      val divResultBuffer = BufferUtils.createFloatBuffer(totalCells)
      val divResultBB = MemoryUtil.memByteBuffer(divResultBuffer)
      
      region.runUnsafe(
        init = FluidState(
          velocity = GBuffer[Vec4[Float32]](totalCells),
          pressure = GBuffer[Float32](totalCells),
          density = GBuffer[Float32](totalCells),
          temperature = GBuffer[Float32](totalCells),
          divergence = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.divergence.read(divResultBB)
      )
      
      println("-" * 60)
      println()
      println("Result buffer (should be 0-9):")
      for i <- 0 until totalCells do
        println(s"  div[$i] = ${divResultBuffer.get(i)}")
      println("=" * 60)
      
    finally
      runtime.close()

