package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}

import java.nio.ByteBuffer
import GProgram.*
import io.computenode.cyfra.core.binding.GUniform
import io.computenode.cyfra.core.binding.GBuffer
import io.computenode.cyfra.core.binding.GBinding
import io.computenode.cyfra.core.expression.{ExpressionBlock, Value}
import izumi.reflect.Tag

import java.io.FileInputStream
import java.nio.file.Path
import scala.util.Using

trait GProgram[Params, L <: Layout: {LayoutBinding, LayoutStruct}] extends GExecution[Params, L, L]:
  val layout: InitProgramLayout => Params => L
  val dispatch: (L, Params) => ProgramDispatch
  val workgroupSize: WorkDimensions
  def layoutStruct: LayoutStruct[L] = summon[LayoutStruct[L]]

object GProgram:
  type WorkDimensions = (Int, Int, Int)

  sealed trait ProgramDispatch
  case class DynamicDispatch[L <: Layout](buffer: GBinding[?], offset: Int) extends ProgramDispatch
  case class StaticDispatch(size: WorkDimensions) extends ProgramDispatch

  def apply[Params, L <: Layout: {LayoutBinding, LayoutStruct}](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    workgroupSize: WorkDimensions = (128, 1, 1),
  )(body: L => ExpressionBlock[Unit]): GProgram[Params, L] =
    new ExpressionProgram[Params, L](body, s => layout(using s), dispatch, workgroupSize)

  def fromSpirvFile[Params, L <: Layout: {LayoutBinding, LayoutStruct}](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    path: Path,
  ): SpirvProgram[Params, L] =
    Using.resource(new FileInputStream(path.toFile)): fis =>
      val fc = fis.getChannel
      val size = fc.size().toInt
      val bb = ByteBuffer.allocateDirect(size)
      fc.read(bb)
      bb.flip()
      SpirvProgram(layout, dispatch, bb)

  private[cyfra] class BufferLengthSpec[T: Value](val length: Int) extends GBuffer[T]:
    private[cyfra] def materialise()(using Allocation): GBuffer[T] = GBuffer.apply[T](length)
  private[cyfra] class DynamicUniform[T: Value]() extends GUniform[T]

  trait InitProgramLayout:
    extension (_buffers: GBuffer.type)
      def apply[T: Value](length: Int): GBuffer[T] =
        BufferLengthSpec[T](length)

    extension (_uniforms: GUniform.type)
      def apply[T: Value](): GUniform[T] =
        DynamicUniform[T]()
      def apply[T: Value](value: T): GUniform[T]
