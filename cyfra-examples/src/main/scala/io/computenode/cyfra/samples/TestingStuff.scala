package io.computenode.cyfra.samples

import io.computenode.cyfra.core.archive.GContext
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.dsl.Value.{GBoolean, Int32}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil
object TestingStuff:

  given GContext = GContext()

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

  @main
  def test =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()

    val emitFilterParams = EmitFilterParams(inSize = 1024, emitN = 2, filterValue = 42)

    val region = GBufferRegion
      .allocate[EmitFilterLayout]
      .map: region =>
        emitFilterExecution.execute(emitFilterParams, region)

    val data = (0 until 1024).toArray
    val buffer = BufferUtils.createByteBuffer(data.length * 4)
    buffer.asIntBuffer().put(data).flip()

    val result = BufferUtils.createIntBuffer(data.length * 2)
    val rbb = MemoryUtil.memByteBuffer(result)
    region.runUnsafe(
      init = EmitFilterLayout(
        inBuffer = GBuffer[Int32](buffer),
        emitBuffer = GBuffer[Int32](data.length * 2),
        filterBuffer = GBuffer[GBoolean](data.length * 2),
      ),
      onDone = layout => layout.filterBuffer.read(rbb),
    )
    runtime.close()

    val actual = (0 until 2 * 1024).map(i => result.get(i * 1) != 0)
    val expected = (0 until 1024).flatMap(x => Seq.fill(emitFilterParams.emitN)(x)).map(_ == emitFilterParams.filterValue)
    expected
      .zip(actual)
      .zipWithIndex
      .foreach:
        case ((e, a), i) => assert(e == a, s"Mismatch at index $i: expected $e, got $a")

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
  )(_ => ???)
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

  @main
  def testAddProgram10Times =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    val bufferSize = 1280
    val params = AddProgramParams(bufferSize, addA = 0, addB = 1)
    val region = GBufferRegion
      .allocate[AddProgramExecLayout]
      .map: region =>
        execution.execute(params, region)

    val inBuffers = List.fill(5)(BufferUtils.createIntBuffer(bufferSize))
    val wbbList = inBuffers.map(MemoryUtil.memByteBuffer)
    val outBuffers = List.fill(5)(BufferUtils.createIntBuffer(bufferSize))
    val rbbList = outBuffers.map(MemoryUtil.memByteBuffer)

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
    runtime.close()
    val expected = inData.map(_ + 11 * (params.addA + params.addB))
    outBuffers.foreach { buf =>
      (0 until bufferSize).foreach { i =>
        assert(buf.get(i) == expected(i), s"Mismatch at index $i: expected ${expected(i)}, got ${buf.get(i)}")
      }
    }

  @main
  def enduranceTest =
    given runtime: VkCyfraRuntime = VkCyfraRuntime()
    val bufferSize = 1280
    val params = AddProgramParams(bufferSize, addA = 0, addB = 1)
    val region = GBufferRegion
      .allocate[AddProgramExecLayout]
      .map: region =>
        execution.execute(params, region)
    (1 to 1000).foreach: _ =>
      val inBuffers = List.fill(5)(BufferUtils.createIntBuffer(bufferSize))
      val wbbList = inBuffers.map(MemoryUtil.memByteBuffer)
      val outBuffers = List.fill(5)(BufferUtils.createIntBuffer(bufferSize))
      val rbbList = outBuffers.map(MemoryUtil.memByteBuffer)

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
    runtime.close()
    println("Endurance test completed successfully")
