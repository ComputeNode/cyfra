package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}
import io.computenode.cyfra.core.GProgram.{InitProgramLayout, ProgramDispatch, WorkDimensions}
import io.computenode.cyfra.core.SpirvProgram.Operation.ReadWrite
import io.computenode.cyfra.core.SpirvProgram.{Binding, ShaderLayout}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.{FromExpr, GBoolean}
import io.computenode.cyfra.dsl.binding.GBinding
import io.computenode.cyfra.dsl.gio.GIO
import izumi.reflect.Tag

import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.Objects
import scala.util.Try
import scala.util.Using
import scala.util.chaining.*

case class SpirvProgram[Params, L <: Layout: {LayoutBinding, LayoutStruct}] private (
  layout: InitProgramLayout => Params => L,
  dispatch: (L, Params) => ProgramDispatch,
  workgroupSize: WorkDimensions,
  code: ByteBuffer,
  entryPoint: String,
  shaderBindings: L => ShaderLayout,
) extends GProgram[Params, L]

object SpirvProgram:
  type ShaderLayout = Seq[Seq[Binding]]
  case class Binding(binding: GBinding[?], operation: Operation)
  enum Operation:
    case Read
    case Write
    case ReadWrite

  def apply[Params, L <: Layout: {LayoutBinding, LayoutStruct}](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    code: ByteBuffer
  ): SpirvProgram[Params, L] =
    val workgroupSize = (128, 1, 1) // TODO Extract form shader
    val main = "main"
    val f: L => ShaderLayout = { case layout: Product =>
      layout.productIterator.zipWithIndex.map { case (binding: GBinding[?], i) => Binding(binding, ReadWrite) }.toSeq.pipe(Seq(_))
    }
    new SpirvProgram[Params, L]((il: InitProgramLayout) => layout(using il), dispatch, workgroupSize, code, main, f)
