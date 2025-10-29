package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.fluids.core.FluidParams
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Simple test: just write constant values to verify GPU writes work */
object SimpleWriteTest:
  
  case class SimpleLayout(
    output: GBuffer[Float32],
    params: GUniform[FluidParams]
  ) extends Layout
  
  val simpleWriteProgram = GProgram[Int, SimpleLayout](
    layout = totalCells => SimpleLayout(
      output = GBuffer[Float32](totalCells),
      params = GUniform[FluidParams]()
    ),
    dispatch = (_, totalCells) => StaticDispatch((totalCells / 256 + 1, 1, 1)),
    workgroupSize = (256, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    // Just write idx as float - simplest possible operation
    GIO.write(layout.output, idx, idx.asFloat * 2.0f)
  
  @main
  def testSimpleWrite(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 60)
      println("Simple Write Test - Verify GPU Writes Work")
      println("=" * 60)
      
      val totalCells = 100
      println(s"Writing to $totalCells cells...")
      
      val region = GBufferRegion
        .allocate[SimpleLayout]
        .map: layout =>
          simpleWriteProgram.execute(totalCells, layout)
      
      // Create params buffer (required even if not used)
      val paramsBuffer = BufferUtils.createByteBuffer(32)
      paramsBuffer.putFloat(0.1f)   // dt
      paramsBuffer.putFloat(0.0f)   // viscosity
      paramsBuffer.putFloat(0.0f)   // diffusion
      paramsBuffer.putFloat(1.0f)   // buoyancy
      paramsBuffer.putFloat(0.0f)   // ambient
      paramsBuffer.putInt(8)         // gridSize
      paramsBuffer.putInt(1)         // iterationCount
      paramsBuffer.flip()
      
      val resultBuffer = BufferUtils.createFloatBuffer(totalCells)
      val resultBB = MemoryUtil.memByteBuffer(resultBuffer)
      
      region.runUnsafe(
        init = SimpleLayout(
          output = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.output.read(resultBB)
      )
      
      // Check results
      var correctCount = 0
      var firstWrong = -1
      for i <- 0 until totalCells do
        val expected = i * 2.0f
        val actual = resultBuffer.get(i)
        if Math.abs(actual - expected) < 0.001f then
          correctCount += 1
        else if firstWrong == -1 then
          firstWrong = i
      
      println(s"Correct values: $correctCount / $totalCells")
      
      if correctCount == totalCells then
        println("✅ ALL WRITES SUCCESSFUL!")
        println()
        println("Sample values:")
        for i <- 0 until Math.min(10, totalCells) do
          println(s"  output[$i] = ${resultBuffer.get(i)} (expected ${i * 2.0f})")
      else
        println(s"❌ FAILED: Only $correctCount values correct")
        if firstWrong != -1 then
          println(s"First wrong at index $firstWrong:")
          println(s"  Expected: ${firstWrong * 2.0f}")
          println(s"  Got: ${resultBuffer.get(firstWrong)}")
        println()
        println("First 10 values:")
        for i <- 0 until Math.min(10, totalCells) do
          println(s"  output[$i] = ${resultBuffer.get(i)} (expected ${i * 2.0f})")
      
      println("=" * 60)
      
    finally
      runtime.close()



