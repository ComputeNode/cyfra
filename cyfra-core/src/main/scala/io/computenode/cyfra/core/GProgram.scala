package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.LayoutStruct
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.core.layout.Layout

import java.nio.ByteBuffer
import GProgram.*
import io.computenode.cyfra.dsl.{Expression, Value}
import io.computenode.cyfra.dsl.Value.{FromExpr, GBoolean, Int32}
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

trait GProgram[Params, L <: Layout: LayoutStruct] extends GExecution[Params, L, L]:
  val layout: InitProgramLayout => Params => L
  val dispatch: (L, Params) => ProgramDispatch
  val workgroupSize: WorkDimensions
  private[cyfra] def layoutStruct: LayoutStruct[L] = summon[LayoutStruct[L]]
  private[cyfra] def cacheKey: String // TODO better type

object GProgram:
  type WorkDimensions = (Int, Int, Int)

  sealed trait ProgramDispatch
  case class DynamicDispatch[L <: Layout](buffer: GBinding[?], offset: Int) extends ProgramDispatch
  case class StaticDispatch(size: WorkDimensions) extends ProgramDispatch
 
  def apply[Params, L <: Layout: LayoutStruct](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    workgroupSize: WorkDimensions = (128, 1, 1),
  )(body: L => GIO[?]): GProgram[Params, L] =
    new GioProgram[Params, L](body, s => layout(using s), dispatch, workgroupSize)

  private[cyfra] class BufferLengthSpec[T <: Value: {Tag, FromExpr}](val length: Int) extends GBuffer[T]:
    private[cyfra] def materialise()(using x: Allocation): GBuffer[T] = GBuffer.apply[T](length)
  private[cyfra] class DynamicUniform[T <: GStruct[T]: {Tag, FromExpr}]() extends GUniform[T]

  trait InitProgramLayout:
    extension (buffers: GBuffer.type)
      def apply[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T] =
        BufferLengthSpec[T](length)

    extension (uniforms: GUniform.type)
      def apply[T <: GStruct[T]: {Tag, FromExpr}](): GUniform[T] =
        DynamicUniform[T]()
      def apply[T <: GStruct[T]: {Tag, FromExpr}](value: T): GUniform[T]
