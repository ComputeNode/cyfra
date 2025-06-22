package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.LayoutStruct
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.core.layout.Layout

import java.nio.ByteBuffer
import GProgram.*
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.buffer.GBuffer
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

class GProgram[Params, Uniform <: GStruct[Uniform], L <: Layout: LayoutStruct] private (
  val body: (L, Uniform) => GIO[?],
  val layout: InitProgramLayout => Params => L,
  val uniform: Params => Uniform,
  val dispatch: (L, Params) => ProgramDispatch,
  val workgroupSize: WorkDimensions,
):
  private[cyfra] def layoutStruct: LayoutStruct[L] = summon[LayoutStruct[L]]

object GProgram:

  type WorkDimensions = (Int, Int, Int)

  sealed trait ProgramDispatch

  case class DynamicDispatch[L <: Layout](buffer: GBuffer[?], offset: Int) extends ProgramDispatch

  case class StaticDispatch(size: WorkDimensions) extends ProgramDispatch

  private[cyfra] case class BufferSizeSpec[T <: Value](size: Int) extends GBuffer[T]

  trait InitProgramLayout:
    extension (buffers: GBuffer.type)
      def apply[T <: Value](size: Int): GBuffer[T] =
        BufferSizeSpec[T](size)

  def apply[Params, Uniform <: GStruct[Uniform], L <: Layout: LayoutStruct](
    layout: InitProgramLayout ?=> Params => L,
    uniform: Params => Uniform,
    dispatch: (L, Params) => ProgramDispatch,
    workgroupSize: WorkDimensions = (128, 1, 1),
  )(body: (L, Uniform) => GIO[?]): GProgram[Params, Uniform, L] =
    new GProgram(body, s => layout(using s), uniform, dispatch, workgroupSize)
