package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GBuffer
import io.computenode.cyfra.dsl.gio.GIO
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Test if printf works at all */
object SimplePrintfTest:
  
  case class TestLayout(output: GBuffer[Float32]) extends Layout
  
  val printfProgram = GProgram[Int, TestLayout](
    layout = totalCells => TestLayout(output = GBuffer[Float32](totalCells)),
    dispatch = (_, totalCells) => StaticDispatch((1, 1, 1)),  // Just 1 workgroup
    workgroupSize = (10, 1, 1)  // 10 threads
  ): layout =>
    val idx = GIO.invocationId
    
    GIO.printf("Thread %d says hello!\n", idx).flatMap: _ =>
      GIO.write(layout.output, idx, idx.asFloat * 2.0f)
  
  @main
  def testSimplePrintf(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 60)
      println("Simple Printf Test")
      println("=" * 60)
      println()
      println("GPU Output:")
      println("-" * 60)
      
      val region = GBufferRegion
        .allocate[TestLayout]
        .map: layout =>
          printfProgram.execute(10, layout)
      
      val resultBuffer = BufferUtils.createFloatBuffer(10)
      val resultBB = MemoryUtil.memByteBuffer(resultBuffer)
      
      region.runUnsafe(
        init = TestLayout(output = GBuffer[Float32](10)),
        onDone = layout => layout.output.read(resultBB)
      )
      
      println("-" * 60)
      println()
      println("Result:")
      for i <- 0 until 10 do
        println(s"  output[$i] = ${resultBuffer.get(i)} (expected ${i * 2.0f})")
      
      println("=" * 60)
      
    finally
      runtime.close()

