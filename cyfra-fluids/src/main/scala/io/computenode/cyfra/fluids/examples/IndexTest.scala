package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.fluids.core.{FluidParams, GridUtils}
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Test 3D indexing and neighbor access */
object IndexTest:
  
  case class IndexLayout(
    input: GBuffer[Float32],
    output: GBuffer[Float32],
    params: GUniform[FluidParams]
  ) extends Layout
  
  // Program that reads from neighbors and sums them
  val indexTestProgram = GProgram[Int, IndexLayout](
    layout = totalCells => IndexLayout(
      input = GBuffer[Float32](totalCells),
      output = GBuffer[Float32](totalCells),
      params = GUniform[FluidParams]()
    ),
    dispatch = (_, totalCells) => StaticDispatch((totalCells / 256 + 1, 1, 1)),
    workgroupSize = (256, 1, 1)
  ): layout =>
    val idx = GIO.invocationId
    val params = layout.params.read
    val n = params.gridSize
    
    // Convert to 3D
    val z = idx / (n * n)
    val y = (idx / n).mod(n)
    val x = idx.mod(n)
    
    // Just write the 3D coordinates encoded as a value
    val encoded = x.asFloat + y.asFloat * 100.0f + z.asFloat * 10000.0f
    
    GIO.write(layout.output, idx, encoded)
  
  @main
  def testIndex(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 60)
      println("Index Test - Verify 3D to 1D Conversion")
      println("=" * 60)
      
      val gridSize = 8
      val totalCells = gridSize * gridSize * gridSize
      
      val region = GBufferRegion
        .allocate[IndexLayout]
        .map: layout =>
          indexTestProgram.execute(totalCells, layout)
      
      val paramsBuffer = BufferUtils.createByteBuffer(32)
      paramsBuffer.putFloat(0.1f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putFloat(1.0f)
      paramsBuffer.putFloat(0.0f)
      paramsBuffer.putInt(gridSize)
      paramsBuffer.putInt(1)
      paramsBuffer.flip()
      
      val resultBuffer = BufferUtils.createFloatBuffer(totalCells)
      val resultBB = MemoryUtil.memByteBuffer(resultBuffer)
      
      region.runUnsafe(
        init = IndexLayout(
          input = GBuffer[Float32](totalCells),
          output = GBuffer[Float32](totalCells),
          params = GUniform[FluidParams](paramsBuffer)
        ),
        onDone = layout => layout.output.read(resultBB)
      )
      
      // Check results
      var correctCount = 0
      for i <- 0 until totalCells do
        val z = i / (gridSize * gridSize)
        val y = (i / gridSize) % gridSize
        val x = i % gridSize
        
        val expected = x + y * 100 + z * 10000
        val actual = resultBuffer.get(i).toInt
        
        if actual == expected then
          correctCount += 1
        else if correctCount < 10 then  // Show first few errors
          println(s"  Error at i=$i: expected ($x,$y,$z)=$expected, got $actual")
      
      println(s"\nCorrect: $correctCount / $totalCells")
      
      if correctCount == totalCells then
        println("✅ 3D indexing works perfectly!")
      else
        println("❌ 3D indexing has errors")
      
      println("=" * 60)
      
    finally
      runtime.close()



