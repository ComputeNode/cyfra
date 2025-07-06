package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.LayoutStruct
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.core.layout.Layout

import java.nio.ByteBuffer
import GProgram.*
import io.computenode.cyfra.dsl.{Expression, Value}
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

class GProgram[Params, L <: Layout: LayoutStruct] private (
  val body: L => GIO[?],
  val layout: InitProgramLayout => Params => L,
  val dispatch: (L, Params) => ProgramDispatch,
  val workgroupSize: WorkDimensions,
) extends GExecution[Params, L, L]:
  private[cyfra] def layoutStruct: LayoutStruct[L] = summon[LayoutStruct[L]]

object GProgram:

  type WorkDimensions = (Int, Int, Int)

  sealed trait ProgramDispatch

  case class DynamicDispatch[L <: Layout](buffer: GBinding, offset: Int) extends ProgramDispatch

  case class StaticDispatch(size: WorkDimensions) extends ProgramDispatch

  private[cyfra] case class BufferSizeSpec[T <: Value: Tag: FromExpr](size: Int) extends GBuffer[T]

  private[cyfra] case class ParamUniform[T <: GStruct[T]: Tag: FromExpr](value: T) extends GUniform[T]

  private[cyfra] case class DynamicUniform[T <: GStruct[T]: Tag: FromExpr]() extends GUniform[T]

  trait InitProgramLayout:
    extension (buffers: GBuffer.type)
      def apply[T <: Value: Tag: FromExpr](size: Int): GBuffer[T] =
        BufferSizeSpec[T](size)

    extension (uniforms: GUniform.type)
      def apply[T <: GStruct[T]: Tag: FromExpr](value: T): GUniform[T] =
        ParamUniform[T](value)

      def apply[T <: GStruct[T]: Tag: FromExpr](): GUniform[T] =
        DynamicUniform[T]()

  def apply[Params, L <: Layout: LayoutStruct](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    workgroupSize: WorkDimensions = (128, 1, 1),
  )(body: L => GIO[?]): GProgram[Params, L] =
    new GProgram(body, s => layout(using s), dispatch, workgroupSize)

