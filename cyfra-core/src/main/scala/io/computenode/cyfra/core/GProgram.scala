package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.gio.GIO

import java.nio.ByteBuffer
import GProgram.*
import io.computenode.cyfra.dsl.{Expression, Value}
import io.computenode.cyfra.dsl.Value.{FromExpr, GBoolean, Int32}
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform, NoProvenance}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

import java.io.FileInputStream
import java.nio.file.Path
import scala.util.Using
import sourcecode.Enclosing

/** A GPU compute program.
  * The dispatch method is PURE - it builds the DAG by returning bindings with provenance.
  * Params are passed at dispatch time, NOT stored in the program.
  */
trait GProgram[Params, L: Layout]:
  val layout: InitProgramLayout => Params => L
  val dispatchSize: (L, Params) => ProgramDispatch
  val workgroupSize: WorkDimensions
  val name: String
  def summonLayout: Layout[L] = Layout[L]

  /** PURE - dispatch builds DAG by creating output bindings with ExecutionNode provenance.
    * Params are passed explicitly, not stored.
    */
  def dispatch(params: Params, input: L): L =
    val layoutInstance = Layout[L]
    val inputBindings = layoutInstance.toBindings(input)

    // Create new bindings with ExecutionNode provenance pointing to this execution
    val outputBindings = inputBindings.zipWithIndex.map { case (binding, idx) =>
      binding.withProvenance(
        Provenance.ExecutionNode(this, params, inputBindings, idx),
      )
    }

    layoutInstance.fromBindings(outputBindings)

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
    val programName = enclosing.value.split('.').dropRight(1).lastOption.getOrElse("Program")
    GioProgram[Params, L](body, s => layout(using s), dispatch, workgroupSize, programName)

  def static[Params, L: Layout](layout: InitProgramLayout ?=> Params => L, dispatchSize: Params => Int)(body: L => GIO[?])(using Enclosing): GProgram[Params, L] =
    val programName = summon[Enclosing].value.split('.').dropRight(1).lastOption.getOrElse("Program")
    GioProgram[Params, L](
      body,
      s => layout(using s),
      (l, p) => StaticDispatch((dispatchSize(p) + 127) / 128, 1, 1),
      (128, 1, 1),
      programName,
    )

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

  import io.computenode.cyfra.dsl.binding.{Provenance as ProvenanceBase}

  private[cyfra] class BufferLengthSpec[T <: Value: {Tag, FromExpr}](
    val length: Int,
    val provenance: ProvenanceBase = NoProvenance,
  ) extends GBuffer[T]:
    override def withProvenance(p: ProvenanceBase): GBuffer[T] = new BufferLengthSpec[T](length, p)

    private[cyfra] def materialise()(using allocation: Allocation): GBuffer[T] = allocation.buffer[T]("spec", length)

  private[cyfra] class DynamicUniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](
    val provenance: ProvenanceBase = NoProvenance,
  ) extends GUniform[T]:
    override def withProvenance(p: ProvenanceBase): GUniform[T] = new DynamicUniform[T](p)

  /** Marker trait for InitProgramLayout context. */
  trait InitProgramLayout:
    def createBuffer[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T] =
        BufferLengthSpec[T](length)
    def createUniform[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](): GUniform[T] =
        DynamicUniform[T]()
    def createUniform[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](value: T): GUniform[T]

  given defaultInitProgramLayout: InitProgramLayout = new InitProgramLayout:
    override def createUniform[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](value: T): GUniform[T] =
      throw new UnsupportedOperationException("createUniform(value) not supported in default context")

  extension (_buffers: GBuffer.type)(using ipl: InitProgramLayout)
    def sized[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T] =
      ipl.createBuffer[T](length)

  extension (_uniforms: GUniform.type)(using ipl: InitProgramLayout)
    def empty[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](): GUniform[T] =
      ipl.createUniform[T]()
    def fromValue[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](value: T): GUniform[T] =
      ipl.createUniform[T](value)
