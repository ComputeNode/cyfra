package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.{GExecution, GProgram, Layout}
import io.computenode.cyfra.core.GCodec.given
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.memory.BuildInVariable.GlobalInvocationId

class ExecutionCacheTest extends munit.FunSuite:
  case class Buffers(input: GBuffer[RuntimeArray[Float32]], output: GBuffer[RuntimeArray[Float32]]) derives Layout

  private val copy = GioProgram[Int, Buffers](
    layout = _ => Buffers(GBuffer(256), GBuffer(256)),
    dispatch = (_, groups) => GProgram.StaticDispatch((groups, 1, 1)),
    workgroupSize = (128, 1, 1),
  ): layout =>
    val index = GIO.read(GlobalInvocationId.focus(_.x))
    GIO.write(layout.output.focus(_.at(index)), GIO.read(layout.input.focus(_.at(index))))

  test("cached executions upload every input update and retain parameter-dependent dispatch"):
    VkCyfraRuntime.using:
      summon[VkCyfraRuntime].withAllocation: allocation =>
        import allocation.*
        val input = GBuffer[Float, Float32](Array.fill(256)(1.0f))
        val output = GBuffer[Float, Float32](Array.fill(256)(0.0f))
        val layout = Buffers(input, output)
        copy.execute(1, layout)
        assertEquals(output.readArray(new Array[Float](256)).toSeq, Seq.fill(128)(1.0f) ++ Seq.fill(128)(0.0f))
        input.writeArray(Array.fill(256)(2.0f))
        copy.execute(2, layout)
        assertEquals(output.readArray(new Array[Float](256)).toSeq, Seq.fill(256)(2.0f))
        input.writeArray(Array.fill(256)(3.0f))
        copy.execute(2, layout)
        assertEquals(output.readArray(new Array[Float](256)).toSeq, Seq.fill(256)(3.0f))
        input.writeArray(Array.fill(256)(4.0f))
        copy.execute(2, layout)
        assertEquals(output.readArray(new Array[Float](256)).toSeq, Seq.fill(256)(4.0f))

  test("queued reuse and DMA copies preserve execution order"):
    val increment = GioProgram[Int, Buffers](
      layout = _ => Buffers(GBuffer(256), GBuffer(256)),
      dispatch = (_, _) => GProgram.StaticDispatch((2, 1, 1)),
      workgroupSize = (128, 1, 1),
    ): layout =>
      val index = GIO.read(GlobalInvocationId.focus(_.x))
      GIO.write(layout.output.focus(_.at(index)), GIO.read(layout.input.focus(_.at(index))) + 1.0f)
    VkCyfraRuntime.using:
      summon[VkCyfraRuntime].withAllocation: allocation =>
        import allocation.*
        val input = GBuffer[Float, Float32](Array.tabulate(256)(_.toFloat))
        val output = GBuffer[Float, Float32](Array.fill(256)(0.0f))
        val layout = Buffers(input, output)
        val execution = increment.addBufferCopy[Int](l => (l.output, l.input), 256 * 4)
        for _ <- 0 until 20 do execution.execute(2, layout)
        assertEquals(output.readArray(new Array[Float](256)).toSeq, (0 until 256).map(_ + 20.0f))

  test("uploaded buffers can be read before any compute submission"):
    VkCyfraRuntime.using:
      summon[VkCyfraRuntime].withAllocation: allocation =>
        import allocation.*
        val input = GBuffer[Float, Float32](Array.fill(256)(7.0f))
        assertEquals(input.readArray(new Array[Float](256)).toSeq, Seq.fill(256)(7.0f))

  test("long command sequences reuse native recording stack space"):
    val execution = (0 until 512).foldLeft[GExecution[Int, Buffers, Buffers]](GExecution[Int, Buffers]()): (acc, i) =>
      acc.addBufferCopy[Int](l => if i % 2 == 0 then (l.input, l.output) else (l.output, l.input), 256 * 4)
    VkCyfraRuntime.using:
      summon[VkCyfraRuntime].withAllocation: allocation =>
        import allocation.*
        val input = GBuffer[Float, Float32](Array.fill(256)(7.0f))
        val output = GBuffer[Float, Float32](Array.fill(256)(0.0f))
        execution.execute(0, Buffers(input, output))
        assertEquals(output.readArray(new Array[Float](256)).toSeq, Seq.fill(256)(7.0f))

  test("array reads and writes use element offsets"):
    VkCyfraRuntime.using:
      summon[VkCyfraRuntime].withAllocation: allocation =>
        import allocation.*
        val input = GBuffer[Float, Float32](Array.fill(256)(0.0f))
        input.writeArray(Array(3.0f, 4.0f), offset = 5)
        assertEquals(input.readArray(new Array[Float](4), offset = 4).toSeq, Seq(0.0f, 3.0f, 4.0f, 0.0f))

  test("barriers cover DMA read-after-write, write-after-read and write-after-write"):
    import ExecutionHandler.{BufferCopyStep, PipelineBarrier, insertBarriers}
    val a = new GProgram.BufferLengthSpec[RuntimeArray[Float32]](256)
    val b = new GProgram.BufferLengthSpec[RuntimeArray[Float32]](256)
    val c = new GProgram.BufferLengthSpec[RuntimeArray[Float32]](256)
    val first = BufferCopyStep(a, b, 1024)
    for next <- Seq(BufferCopyStep(b, c, 1024), BufferCopyStep(c, a, 1024), BufferCopyStep(c, b, 1024)) do
      assertEquals(insertBarriers(Seq(first, next)), Seq(first, PipelineBarrier, next))
    val independent = BufferCopyStep(a, c, 1024)
    assertEquals(insertBarriers(Seq(first, independent)), Seq(first, independent))
