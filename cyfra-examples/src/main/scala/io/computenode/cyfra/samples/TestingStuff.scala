package io.computenode.cyfra.samples

import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.archive.GContext
import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GExecution, GProgram, SpirvProgram}
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.Value.{GBoolean, Int32}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}
import org.lwjgl.BufferUtils
import io.computenode.cyfra.runtime.VkCyfraRuntime
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
    dispatch = (layout, args) => GProgram.StaticDispatch((args.inSize, 1, 1)),
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
    dispatch = (layout, args) => GProgram.StaticDispatch((args.inSize, 1, 1)),
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

    val emit = SpirvProgram[EmitProgramParams, EmitProgramLayout](
      "emit.spv",
      layout = (il: InitProgramLayout) ?=> emitProgram.layout(il),
      dispatch = emitProgram.dispatch,
    )

    val filter = SpirvProgram[FilterProgramParams, FilterProgramLayout](
      "filter.spv",
      layout = (il: InitProgramLayout) ?=> filterProgram.layout(il),
      dispatch = filterProgram.dispatch,
    )

    runtime.getOrLoadProgram(emit)
    runtime.getOrLoadProgram(filter)

    val emitFilterParams = EmitFilterParams(inSize = 1024, emitN = 2, filterValue = 42)

    val region = GBufferRegion
      .allocate[EmitFilterLayout]
      .map: region =>
        emitFilterExecution.execute(emitFilterParams, region)

    val data = (0 until 1024).toArray
    val buffer = BufferUtils.createByteBuffer(data.length * 4)
    buffer.asIntBuffer().put(data).flip()

    val result = BufferUtils.createByteBuffer(data.length * 2)
    region.runUnsafe(
      init = EmitFilterLayout(
        inBuffer = GBuffer[Int32](buffer),
        emitBuffer = GBuffer[Int32](data.length * 2),
        filterBuffer = GBuffer[GBoolean](data.length * 2),
      ),
      onDone = layout => layout.filterBuffer.read(result),
    )

    val actual = (0 until 2 * 1024).map(i => result.get(i * 1) != 0)
    val expected = (0 until 1024).flatMap(x => Seq.fill(emitFilterParams.emitN)(x)).map(_ == 42)
    expected
      .zip(actual)
      .zipWithIndex
      .foreach:
        case ((e, a), i) => assert(e == a, s"Mismatch at index $i: expected $e, got $a")
