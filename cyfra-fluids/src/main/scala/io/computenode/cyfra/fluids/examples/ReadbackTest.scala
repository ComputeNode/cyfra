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

/** Test: write input data, read it back to verify it's correct */
object ReadbackTest:
  
  case class SimpleLayout(
    input: GBuffer[Vec3[Float32]],
    output: GBuffer[Vec3[Float32]],
    params: GUniform[FluidParams]
  ) extends Layout
  
  val readbackProgram = GProgram[Int, SimpleLayout](
    layout = totalCells => SimpleLayout(
      input = GBuffer[Vec3[Float32]](totalCells),
      output = GBuffer[Vec3[Float32]](totalCells),
      params = GUniform[FluidParams]()
    ),
    dispatch = (_, totalCells) => StaticDispatch((totalCells / 256 + 1, 1, 1)),
    workgroupSize = (256, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    // Just copy input to output
    val vel = GIO.read(layout.input, idx)
    GIO.write(layout.output, idx, vel)
  
  @main
  def testReadback(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 60)
      println("Readback Test - Verify Data Transfer")
      println("=" * 60)
      
      val totalCells = 100
      
      // Create test data
      val velData = Array.ofDim[Float](totalCells * 3)
      for i <- 0 until totalCells do
        velData(i * 3 + 0) = i.toFloat      // x
        velData(i * 3 + 1) = i * 10.0f      // y
        velData(i * 3 + 2) = i * 100.0f     // z
      
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
      
      val region = GBufferRegion
        .allocate[SimpleLayout]
        .map: layout =>
          readbackProgram.execute(totalCells, layout)
      
      val resultBuffer = BufferUtils.createFloatBuffer(totalCells * 3)
      val resultBB = MemoryUtil.memByteBuffer(resultBuffer)
      
      region.runUnsafe(
        init = SimpleLayout(
          input = GBuffer[Vec3[Float32]](velBuffer),
          output = GBuffer[Vec3[Float32]](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.output.read(resultBB)
      )
      
      // Check
      var correctCount = 0
      for i <- 0 until totalCells do
        val expectedX = i.toFloat
        val expectedY = i * 10.0f
        val expectedZ = i * 100.0f
        
        val actualX = resultBuffer.get(i * 3 + 0)
        val actualY = resultBuffer.get(i * 3 + 1)
        val actualZ = resultBuffer.get(i * 3 + 2)
        
        if Math.abs(actualX - expectedX) < 0.001f &&
           Math.abs(actualY - expectedY) < 0.001f &&
           Math.abs(actualZ - expectedZ) < 0.001f then
          correctCount += 1
      
      println(s"Correct: $correctCount / $totalCells")
      println()
      
      if correctCount == totalCells then
        println("✅ Data transfer works perfectly!")
      else
        println("❌ Data transfer has errors")
        println("\nFirst few cells:")
        for i <- 0 until Math.min(5, totalCells) do
          val ex = i.toFloat
          val ey = i * 10.0f
          val ez = i * 100.0f
          val ax = resultBuffer.get(i * 3 + 0)
          val ay = resultBuffer.get(i * 3 + 1)
          val az = resultBuffer.get(i * 3 + 2)
          println(s"  Cell $i: expected=($ex,$ey,$ez), actual=($ax,$ay,$az)")
      
      println("=" * 60)
      
    finally
      runtime.close()

