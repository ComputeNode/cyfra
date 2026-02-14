package io.computenode.cyfra.samples

import io.computenode.cyfra.core.{Allocation, GProgram}
import io.computenode.cyfra.core.GProgram.{*, given}
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import org.lwjgl.BufferUtils

object TestingStuff:

  // === Simple emit program that duplicates each element ===

  case class EmitParams(inSize: Int, emitN: Int)

  case class EmitLayout(
    in: GBuffer[Int32],
    out: GBuffer[Int32],
  ) derives Layout

  def emitProgram: GProgram[EmitParams, EmitLayout] =
    GProgram[EmitParams, EmitLayout](
      layout = p =>
        EmitLayout(
          in = GBuffer.sized[Int32](p.inSize),
          out = GBuffer.sized[Int32](p.inSize * p.emitN),
        ),
      dispatch = (_, p) => GProgram.StaticDispatch(((p.inSize + 127) / 128, 1, 1)),
    ): layout =>
      val params = summon[InitProgramLayout] // Not used directly, but we can get params via dispatch
      val invocId = GIO.invocationId
      // Note: We can't access params inside the body directly since it's not a given here
      // The body only sees the layout. Params affect layout creation and dispatch size.
      // For accessing runtime params, we'd need a uniform.
      val element = GIO.read(layout.in, invocId)
      // For emit, we'd need to pass emitN as a uniform or hardcode it
      val emitN: Int32 = 2 // Hardcoded for this example
      val bufferOffset = invocId * emitN
      GIO.repeat(2): i =>
        GIO.write(layout.out, bufferOffset + i, element)

  @main
  def testEmit(): Unit =
    val runtime = VkCyfraRuntime()

    val params = EmitParams(inSize = 1024, emitN = 2)
    val program = emitProgram

    val data = (0 until 1024).toArray
    val inputBuffer = BufferUtils.createByteBuffer(data.length * 4)
    inputBuffer.asIntBuffer().put(data).flip()

    runtime.withAllocation { allocation =>
      given Allocation = allocation

      // Create input and output buffers
      val layout = EmitLayout(
        in = allocation.buffer[Int32](inputBuffer),
        out = allocation.buffer[Int32](params.inSize * params.emitN),
      )

      // Dispatch the program (builds DAG)
      val outputLayout = program.dispatch(params, layout)

      // Materialize to execute
      allocation.materialize(outputLayout)

      // Read results
      val resultBuffer = BufferUtils.createByteBuffer(params.inSize * params.emitN * 4)
      outputLayout.out.readTo(resultBuffer)
      resultBuffer.rewind()

      val result = new Array[Int](params.inSize * params.emitN)
      resultBuffer.asIntBuffer().get(result)

      // Verify
      val expected = (0 until 1024).flatMap(x => Seq.fill(params.emitN)(x)).toArray
      expected.zip(result).zipWithIndex.foreach:
        case ((e, a), i) =>
          assert(e == a, s"Mismatch at index $i: expected $e, got $a")

      println("Test passed!")
    }

    runtime.close()
