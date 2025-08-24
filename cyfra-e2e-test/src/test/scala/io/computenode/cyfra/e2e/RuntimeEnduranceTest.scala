package io.computenode.cyfra.e2e

import io.computenode.cyfra.core.archive.GContext
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.dsl.Value.{GBoolean, Int32}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.utility.Logger.logger
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Await, Future}


class RuntimeEnduranceTest extends munit.FunSuite:

  test("Endurance test for GExecution with multiple programs"):
    runEnduranceTest(10000)

  // === Emit program ===

  case class EmitProgramParams(inSize: Int, emitN: Int)

  case class EmitProgramUniform(emitN: Int32) extends GStruct[EmitProgramUniform]

  case class EmitProgramLayout(
    in: GBuffer[Int32],
    out: GBuffer[Int32],
    args: GUniform[EmitProgramUniform] = GUniform.fromParams, // todo will be different in the future
  ) extends Layout

  val emitProgram = GProgram[EmitProgramParams, EmitProgramLayout](
    layout = params =>
      EmitProgramLayout(
        in = GBuffer[Int32](params.inSize),
        out = GBuffer[Int32](params.inSize * params.emitN),
        args = GUniform(EmitProgramUniform(params.emitN)),
      ),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val EmitProgramUniform(emitN) = layout.args.read
    val invocId = GIO.invocationId
    val element = GIO.read(layout.in, invocId)
    val bufferOffset = invocId * emitN
    GIO.repeat(emitN): i =>
      GIO.write(layout.out, bufferOffset + i, element)

  // === Filter program ===

  case class FilterProgramParams(inSize: Int, filterValue: Int)

  case class FilterProgramUniform(filterValue: Int32) extends GStruct[FilterProgramUniform]

  case class FilterProgramLayout(in: GBuffer[Int32], out: GBuffer[GBoolean], params: GUniform[FilterProgramUniform] = GUniform.fromParams)
    extends Layout

  val filterProgram = GProgram[FilterProgramParams, FilterProgramLayout](
    layout = params =>
      FilterProgramLayout(
        in = GBuffer[Int32](params.inSize),
        out = GBuffer[GBoolean](params.inSize),
        params = GUniform(FilterProgramUniform(params.filterValue)),
      ),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val invocId = GIO.invocationId
    val element = GIO.read(layout.in, invocId)
    val isMatch = element === layout.params.read.filterValue
    GIO.write(layout.out, invocId, isMatch)

  // === GExecution ===

  case class EmitFilterParams(inSize: Int, emitN: Int, filterValue: Int)

  case class EmitFilterLayout(inBuffer: GBuffer[Int32], emitBuffer: GBuffer[Int32], filterBuffer: GBuffer[GBoolean]) extends Layout

  case class EmitFilterResult(out: GBuffer[GBoolean]) extends Layout

  val emitFilterExecution = GExecution[EmitFilterParams, EmitFilterLayout]()
    .addProgram(emitProgram)(
      params => EmitProgramParams(inSize = params.inSize, emitN = params.emitN),
      layout => EmitProgramLayout(in = layout.inBuffer, out = layout.emitBuffer),
    )
    .addProgram(filterProgram)(
      params => FilterProgramParams(inSize = 2 * params.inSize, filterValue = params.filterValue),
      layout => FilterProgramLayout(in = layout.emitBuffer, out = layout.filterBuffer),
    )

  // Test case: Use one program 10 times, copying values from five input buffers to five output buffers and adding values from two uniforms
  case class AddProgramParams(bufferSize: Int, addA: Int, addB: Int)

  case class AddProgramUniform(a: Int32) extends GStruct[AddProgramUniform]

  case class AddProgramLayout(
    in1: GBuffer[Int32],
    in2: GBuffer[Int32],
    in3: GBuffer[Int32],
    in4: GBuffer[Int32],
    in5: GBuffer[Int32],
    out1: GBuffer[Int32],
    out2: GBuffer[Int32],
    out3: GBuffer[Int32],
    out4: GBuffer[Int32],
    out5: GBuffer[Int32],
    u1: GUniform[AddProgramUniform] = GUniform.fromParams,
    u2: GUniform[AddProgramUniform] = GUniform.fromParams,
  ) extends Layout

  case class AddProgramExecLayout(
    in1: GBuffer[Int32],
    in2: GBuffer[Int32],
    in3: GBuffer[Int32],
    in4: GBuffer[Int32],
    in5: GBuffer[Int32],
    out1: GBuffer[Int32],
    out2: GBuffer[Int32],
    out3: GBuffer[Int32],
    out4: GBuffer[Int32],
    out5: GBuffer[Int32],
  ) extends Layout

  val addProgram: GProgram[AddProgramParams, AddProgramLayout] = GProgram[AddProgramParams, AddProgramLayout](
    layout = params =>
      AddProgramLayout(
        in1 = GBuffer[Int32](params.bufferSize),
        in2 = GBuffer[Int32](params.bufferSize),
        in3 = GBuffer[Int32](params.bufferSize),
        in4 = GBuffer[Int32](params.bufferSize),
        in5 = GBuffer[Int32](params.bufferSize),
        out1 = GBuffer[Int32](params.bufferSize),
        out2 = GBuffer[Int32](params.bufferSize),
        out3 = GBuffer[Int32](params.bufferSize),
        out4 = GBuffer[Int32](params.bufferSize),
        out5 = GBuffer[Int32](params.bufferSize),
        u1 = GUniform(AddProgramUniform(params.addA)),
        u2 = GUniform(AddProgramUniform(params.addB)),
      ),
    dispatch = (layout, args) => GProgram.StaticDispatch((args.bufferSize / 128, 1, 1)),
  ):
    case AddProgramLayout(in1, in2, in3, in4, in5, out1, out2, out3, out4, out5, u1, u2) =>
      val index = GIO.invocationId
      val a = u1.read.a
      val b = u2.read.a
      for
        _ <- GIO.write(out1, index, GIO.read(in1, index) + a + b)
        _ <- GIO.write(out2, index, GIO.read(in2, index) + a + b)
        _ <- GIO.write(out3, index, GIO.read(in3, index) + a + b)
        _ <- GIO.write(out4, index, GIO.read(in4, index) + a + b)
        _ <- GIO.write(out5, index, GIO.read(in5, index) + a + b)
      yield Empty()

  def swap(l: AddProgramLayout): AddProgramLayout =
    val AddProgramLayout(in1, in2, in3, in4, in5, out1, out2, out3, out4, out5, u1, u2) = l
    AddProgramLayout(out1, out2, out3, out4, out5, in1, in2, in3, in4, in5, u1, u2)

  def fromExecLayout(l: AddProgramExecLayout): AddProgramLayout =
    val AddProgramExecLayout(in1, in2, in3, in4, in5, out1, out2, out3, out4, out5) = l
    AddProgramLayout(in1, in2, in3, in4, in5, out1, out2, out3, out4, out5)

  val execution = (0 until 11).foldLeft(
    GExecution[AddProgramParams, AddProgramExecLayout]().asInstanceOf[GExecution[AddProgramParams, AddProgramExecLayout, AddProgramExecLayout]],
  )((x, i) =>
    if i % 2 == 0 then x.addProgram(addProgram)(mapParams = identity[AddProgramParams], mapLayout = fromExecLayout)
    else x.addProgram(addProgram)(mapParams = identity, mapLayout = x => swap(fromExecLayout(x))),
  )

  def runEnduranceTest(nRuns: Int): Unit =
    logger.info(s"Starting endurance test with ${nRuns} runs...")

    given runtime: VkCyfraRuntime = VkCyfraRuntime()

    val bufferSize = 1280
    val params = AddProgramParams(bufferSize, addA = 0, addB = 1)
    val region = GBufferRegion
      .allocate[AddProgramExecLayout]
      .map: region =>
        execution.execute(params, region)
    val aInt = new AtomicInteger(0)
    val runs = (1 to nRuns).map:
      i => Future:
        val inBuffers = List.fill(5)(BufferUtils.createIntBuffer(bufferSize))
        val wbbList = inBuffers.map(MemoryUtil.memByteBuffer)
        val rbbList = List.fill(5)(BufferUtils.createByteBuffer(bufferSize * 4))

        val inData = (0 until bufferSize).toArray
        inBuffers.foreach(_.put(inData).flip())
        region.runUnsafe(
          init = AddProgramExecLayout(
            in1 = GBuffer[Int32](wbbList(0)),
            in2 = GBuffer[Int32](wbbList(1)),
            in3 = GBuffer[Int32](wbbList(2)),
            in4 = GBuffer[Int32](wbbList(3)),
            in5 = GBuffer[Int32](wbbList(4)),
            out1 = GBuffer[Int32](bufferSize),
            out2 = GBuffer[Int32](bufferSize),
            out3 = GBuffer[Int32](bufferSize),
            out4 = GBuffer[Int32](bufferSize),
            out5 = GBuffer[Int32](bufferSize),
          ),
          onDone = layout => {
            layout.out1.read(rbbList(0))
            layout.out2.read(rbbList(1))
            layout.out3.read(rbbList(2))
            layout.out4.read(rbbList(3))
            layout.out5.read(rbbList(4))
          },
        )
        val prev = aInt.getAndAdd(1)
        if prev % 50 == 0 then logger.info(s"Iteration $prev completed")

    val allRuns = Future.sequence(runs)
    Await.result(allRuns, scala.concurrent.duration.Duration.Inf)

    runtime.close()
    logger.info("Endurance test completed successfully")
