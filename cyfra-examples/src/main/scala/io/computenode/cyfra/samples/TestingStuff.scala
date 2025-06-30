package io.computenode.cyfra.samples

import io.computenode.cyfra.core.aalegacy.GContext
import io.computenode.cyfra.core.binding.ParamUniform
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.layout.*
import io.computenode.cyfra.dsl.Value.{GBoolean, Int32}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}
import org.lwjgl.BufferUtils

object TestingStuff:

  given GContext = GContext()

  // === Emit program ===

  case class EmitProgramParams(inSize: Int, emitN: Int)

  case class EmitProgramLayout(in: GBuffer[Int32], out: GBuffer[Int32]) extends Layout

  case class EmitProgramUniform(emitN: Int32) extends GStruct[EmitProgramUniform]

  // 1 2 3 4
  // VVVV (inSize = 4, emitN = 3)
  // 1 1 1 2 2 2 3 3 3 4 4 4

  val emitProgram = GProgram[EmitProgramParams, EmitProgramLayout](
    layout = params => EmitProgramLayout(in = GBuffer[Int32](params.inSize), out = GBuffer[Int32](params.inSize * params.emitN)),
    dispatch = (layout, args) => GProgram.StaticDispatch((args.inSize, 1, 1)),
  ): (layout) =>
    val invocId = GIO.invocationId
    val element = GIO.read(layout.in, invocId)
    val bufferOffset = invocId * args.emitN
    GIO.repeat(args.emitN): i =>
      GIO.write(layout.out, bufferOffset + i, element)

  // === Filter program ===
  case class FilterProgramParams(inSize: Int, filterValue: Int)

  case class FilterProgramUniform(filterValue: Int32) extends GStruct[FilterProgramUniform]

  case class FilterProgramLayout(
    in: GBuffer[Int32], 
    out: GBuffer[GBoolean], 
    params: GUniform[FilterProgramUniform]
  ) extends Layout

  val filterProgram = GProgram[FilterProgramParams, FilterProgramLayout](
    layout = params => FilterProgramLayout(
      in = GBuffer[Int32](params.inSize), 
      out = GBuffer[GBoolean](params.inSize),
      params = ParamUniform(FilterProgramUniform(params.filterValue))
    ),
    dispatch = (layout, args) => GProgram.StaticDispatch((args.inSize, 1, 1)),
  ): layout =>
    val invocId = GIO.invocationId
    val element = GIO.read(layout.in, invocId)
    val isMatch = element === layout.params.filterValue
    GIO.write(layout.out, invocId, isMatch)

  // === GExecution ===

  case class EmitFilterParams(inSize: Int, emitN: Int, filterValue: Int)

  case class EmitFilterLayout(inBuffer: GBuffer[Int32], emitBuffer: GBuffer[Int32], filterBuffer: GBuffer[GBoolean]) extends Layout

  case class EmitFilterResult(out: GBuffer[GBoolean]) extends Layout

  val emitFilterExecution = GExecution
    .build[EmitFilterParams, EmitFilterLayout, EmitFilterResult]
    .addProgram(emitProgram)( // (inBuffer, emitBuffer)
      mapLayout = layout => EmitProgramLayout(in = layout.inBuffer, out = layout.emitBuffer),
      mapParams = params => EmitProgramParams(inSize = params.inSize, emitN = params.emitN),
    )
    .addProgram(filterProgram)( // (emitBuffer, filterBuffer)
      mapLayout = layout => FilterProgramLayout(in = layout.emitBuffer, out = layout.filterBuffer),
      mapParams = params => FilterProgramParams(inSize = params.inSize * params.emitN, filterValue = params.filterValue),
    )
    .compile(layout => EmitFilterResult(layout.filterBuffer))
  
  @main
  def test =

    val emitFilterParams = EmitFilterParams(inSize = 1024, emitN = 2, filterValue = 42)

    val region = GBufferRegion
      .allocate[EmitFilterLayout]
      .map: region =>
        emitFilterExecution.execute(region, emitFilterParams)

    val data = (0 to 1024).toArray
    val buffer = BufferUtils.createByteBuffer(data.length * 4)
    buffer.asIntBuffer().put(data).flip()
    

    val result = BufferUtils.createByteBuffer(data.length * 2)
    region.runUnsafe(
      init = EmitFilterLayout(
        inBuffer = GBuffer[Int32](buffer),
        emitBuffer = GBuffer[Int32](data.length * 2),
        filterBuffer = GBuffer[GBoolean](data.length * 2),
      ),
      onDone = layout => layout.out.readTo(result),
    )
