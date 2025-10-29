package io.computenode.cyfra.fluids.examples

import io.computenode.cyfra.core.{GBufferRegion, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GBuffer
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

/** Simple test to see if printf works at all */
object SimplePrintTest:
  
  case class TestLayout(output: GBuffer[Float32]) extends Layout
  
  val simpleProgram = GProgram[Unit, TestLayout](
    layout = _ => TestLayout(output = GBuffer[Float32](10)),
    dispatch = (_, _) => StaticDispatch((1, 1, 1)),  // Just 1 workgroup
    workgroupSize = (10, 1, 1)  // 10 threads
  ): layout =>
    val idx = GIO.invocationId
    
    // Simple write without printf for now
    GIO.write(layout.output, idx, idx.asFloat * 2.0f)
  
  @main
  def testSimplePrint(): Unit =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    
    try
      println("=" * 70)
      println("Simple Printf Test")
      println("=" * 70)
      println()
      
      val region = GBufferRegion
        .allocate[TestLayout]
        .map: layout =>
          simpleProgram.execute((), layout)
      
      // Initialize output buffer with zeros
      val outputData = Array.fill(10)(0.0f)
      val outputBuffer = BufferUtils.createByteBuffer(outputData.length * 4)
      outputBuffer.asFloatBuffer().put(outputData).flip()
      
      val resultBuffer = BufferUtils.createFloatBuffer(10)
      val resultBB = MemoryUtil.memByteBuffer(resultBuffer)
      
      region.runUnsafe(
        init = TestLayout(output = GBuffer[Float32](outputBuffer)),
        onDone = layout => layout.output.read(resultBB)
      )
      
      println()
      println("Results:")
      for i <- 0 until 10 do
        println(s"  output[$i] = ${resultBuffer.get(i)}")
      println()
      println("=" * 70)
      
    finally
      runtime.close()


