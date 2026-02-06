package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.gio.GIO

import java.nio.ByteBuffer
import GProgram.*
import io.computenode.cyfra.dsl.{Expression, Value}
import io.computenode.cyfra.dsl.Value.{FromExpr, GBoolean, Int32}
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

import java.io.FileInputStream
import java.nio.file.Path
import scala.util.Using
import sourcecode.Enclosing

trait GProgram[Params, L: Layout] extends GExecution[Params, L, L]:
  val layout: InitProgramLayout => Params => L
  val dispatch: (L, Params) => ProgramDispatch
  val workgroupSize: WorkDimensions
  val name: String
  def summonLayout: Layout[L] = Layout[L]

object GProgram:
  type WorkDimensions = (Int, Int, Int)

  sealed trait ProgramDispatch
  case class DynamicDispatch[L: Layout](buffer: GBinding[?], offset: Int) extends ProgramDispatch
  case class StaticDispatch(size: WorkDimensions) extends ProgramDispatch

  def apply[Params, L: Layout](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    workgroupSize: WorkDimensions = (128, 1, 1),
  )(body: L => GIO[?])(using enclosing: Enclosing): GProgram[Params, L] =
    // Extract program name from enclosing context (e.g. "pkg.F16RMSNormProgram.forward" -> "F16RMSNormProgram")
    val programName = enclosing.value.split('.').dropRight(1).lastOption.getOrElse("Program")
    new GioProgram[Params, L](body, s => layout(using s), dispatch, workgroupSize, programName)

  def static[Params, L: Layout](layout: InitProgramLayout ?=> Params => L, dispatchSize: Params => Int)(body: L => GIO[?])(using Enclosing): GProgram[Params, L] =
    val programName = summon[Enclosing].value.split('.').dropRight(1).lastOption.getOrElse("Program")
    GioProgram.apply(body, s => layout(using s), (l, p) => StaticDispatch((dispatchSize(p) + 127) / 128, 1, 1), (128, 1, 1), programName)

  def fromSpirvFile[Params, L: Layout](
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

  private[cyfra] class BufferLengthSpec[T <: Value: {Tag, FromExpr}](val length: Int) extends GBuffer[T]:
    private[cyfra] def materialise()(using Allocation): GBuffer[T] = GBuffer.apply[T](length)
  private[cyfra] class DynamicUniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}]() extends GUniform[T]

  trait InitProgramLayout:
    extension (_buffers: GBuffer.type)
      def apply[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T] =
        BufferLengthSpec[T](length)

    extension (_uniforms: GUniform.type)
      def apply[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](): GUniform[T] =
        DynamicUniform[T]()
      def apply[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](value: T): GUniform[T]
