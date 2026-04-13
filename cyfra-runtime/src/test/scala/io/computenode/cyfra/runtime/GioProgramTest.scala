package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.expression.ops.*
import io.computenode.cyfra.core.expression.ops.given
import io.computenode.cyfra.dsl.Library.*
import io.computenode.cyfra.core.memory.*
import io.computenode.cyfra.core.expression.JumpTarget.BreakTarget
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.memory.BuildInVariable.GlobalInvocationId
import io.computenode.cyfra.dsl.direct.*
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram, Layout}
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryUtil

class GioProgramTest extends munit.FunSuite:

  var runtime: VkCyfraRuntime = scala.compiletime.uninitialized
  given VkCyfraRuntime = runtime

  override def beforeAll(): Unit =
    runtime = VkCyfraRuntime()

  override def afterAll(): Unit =
    if runtime != null then runtime.close()

  // === Emit program ===

  case class EmitProgramParams(inSize: Int, emitN: Int)
  case class EmitProgramUniform(emitN: UInt32)
  case class EmitProgramLayout(
    in: GBuffer[GArray[UInt32]],
    out: GBuffer[GArray[UInt32]],
    args: GUniform[EmitProgramUniform] = GUniform.fromParams,
  )

  val emitProgram = GioProgram[EmitProgramParams, EmitProgramLayout](
    layout = params =>
      EmitProgramLayout(
        in = GBuffer(params.inSize),
        out = GBuffer(params.inSize * params.emitN),
        args = GUniform(EmitProgramUniform(UInt32(params.emitN))),
      ),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val invocId = GIO.read(GlobalInvocationId.focus(_.x))
    val params  = GIO.read(layout.args)
    val element = GIO.read[UInt32](layout.in.focus(_.at(invocId)))
    val bufferOffset = invocId * params.emitN
    val iV: Variable[UInt32] = GIO.declare()
    GIO.write(iV, UInt32(0))
    val body: (BreakTarget, GIO) ?=> Unit =
      val i = GIO.read(iV)
      GIO.conditionalBreak(i === params.emitN)
      GIO.write[UInt32](layout.out.focus(_.at(bufferOffset + i)), element)
    val continue: GIO ?=> Unit =
      val i = GIO.read(iV)
      GIO.write(iV, i + UInt32(1))
    GIO.loop(body, continue)

  // === Filter program ===

  case class FilterProgramParams(inSize: Int, filterValue: Int)
  case class FilterProgramUniform(filter: UInt32)
  case class FilterProgramLayout(
    in: GBuffer[GArray[UInt32]],
    out: GBuffer[GArray[UInt32]],
    params: GUniform[FilterProgramUniform] = GUniform.fromParams,
  )

  val filterProgram = GioProgram[FilterProgramParams, FilterProgramLayout](
    layout = params =>
      FilterProgramLayout(
        in = GBuffer(params.inSize),
        out = GBuffer(params.inSize),
        params = GUniform(FilterProgramUniform(UInt32(params.filterValue))),
      ),
    dispatch = (_, args) => GProgram.StaticDispatch((args.inSize / 128, 1, 1)),
  ): layout =>
    val invocId = GIO.read(GlobalInvocationId.focus(_.x))
    val element = GIO.read(layout.in.focus(_.at(invocId)))
    val uniform = GIO.read(layout.params)
    GIO.write(layout.out.focus(_.at(invocId)), when(element === uniform.filter)(UInt32(1))(UInt32(0)))

  // === Chained execution ===

  case class EmitFilterParams(inSize: Int, emitN: Int, filterValue: Int)
  case class EmitFilterLayout(
    inBuffer: GBuffer[GArray[UInt32]],
    emitBuffer: GBuffer[GArray[UInt32]],
    filterBuffer: GBuffer[GArray[UInt32]],
  )

  val emitFilterExecution = GExecution[EmitFilterParams, EmitFilterLayout]()
    .addProgram(emitProgram)(
      params => EmitProgramParams(inSize = params.inSize, emitN = params.emitN),
      layout => EmitProgramLayout(in = layout.inBuffer, out = layout.emitBuffer),
    )
    .addProgram(filterProgram)(
      params => FilterProgramParams(inSize = params.inSize * params.emitN, filterValue = params.filterValue),
      layout => FilterProgramLayout(in = layout.emitBuffer, out = layout.filterBuffer),
    )

  // === Tests ===

  test("emit duplicates each element emitN times"):
    val inSize = 1024
    val emitN  = 2
    val params = EmitProgramParams(inSize = inSize, emitN = emitN)

    val inBuffer = BufferUtils.createByteBuffer(inSize * 4)
    inBuffer.asIntBuffer().put((0 until inSize).toArray).flip()

    val outBuffer = BufferUtils.createIntBuffer(inSize * emitN)

    GBufferRegion
      .allocate[EmitProgramLayout]
      .map(emitProgram.execute(params, _))
      .runUnsafe(
        init   = EmitProgramLayout(in = GBuffer(inBuffer), out = GBuffer(inSize * emitN)),
        onDone = _.out.read(MemoryUtil.memByteBuffer(outBuffer)),
      )

    val actual   = (0 until inSize * emitN).map(outBuffer.get)
    val expected = (0 until inSize).flatMap(x => Seq.fill(emitN)(x))
    assertEquals(actual, expected)

  test("emit then filter retains only matching elements"):
    val inSize      = 1024
    val emitN       = 2
    val filterValue = 42
    val params      = EmitFilterParams(inSize = inSize, emitN = emitN, filterValue = filterValue)

    val inBuffer = BufferUtils.createByteBuffer(inSize * 4)
    inBuffer.asIntBuffer().put((0 until inSize).toArray).flip()

    val outBuffer = BufferUtils.createIntBuffer(inSize * emitN)

    GBufferRegion
      .allocate[EmitFilterLayout]
      .map(emitFilterExecution.execute(params, _))
      .runUnsafe(
        init = EmitFilterLayout(
          inBuffer     = GBuffer(inBuffer),
          emitBuffer   = GBuffer(inSize * emitN),
          filterBuffer = GBuffer(inSize * emitN),
        ),
        onDone = _.filterBuffer.read(MemoryUtil.memByteBuffer(outBuffer)),
      )

    val actual   = (0 until inSize * emitN).map(outBuffer.get(_) != 0)
    val expected = (0 until inSize).flatMap(x => Seq.fill(emitN)(x)).map(_ == filterValue)
    assertEquals(actual, expected)
