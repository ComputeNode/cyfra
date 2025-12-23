package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}
import io.computenode.cyfra.core.GProgram.{InitProgramLayout, ProgramDispatch, WorkDimensions}
import io.computenode.cyfra.core.SpirvProgram.Operation.ReadWrite
import io.computenode.cyfra.core.SpirvProgram.{Binding, ShaderLayout}
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.binding.GBinding
import izumi.reflect.Tag

import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.security.MessageDigest
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
) extends GProgram[Params, L]:

  /** A hash of the shader code, entry point, workgroup size, and layout bindings. Layout and dispatch are not taken into account.
    */
  lazy val shaderHash: (Long, Long) =
    val md = MessageDigest.getInstance("SHA-256")
    md.update(code)
    code.rewind()
    md.update(entryPoint.getBytes)
    md.update(
      workgroupSize.toList
        .flatMap(BigInt(_).toByteArray)
        .toArray,
    )
    val layout = shaderBindings(summon[LayoutStruct[L]].layoutRef)
    layout.flatten.foreach: binding =>
//      md.update(binding.binding.tag.toString.getBytes)
      md.update(binding.operation.toString.getBytes)
    val digest = md.digest()
    val bb = java.nio.ByteBuffer.wrap(digest)
    (bb.getLong(), bb.getLong())

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
    code: ByteBuffer,
  ): SpirvProgram[Params, L] =
    val workgroupSize = (128, 1, 1) // TODO  Extract form shader
    val main = "main"
    val f: L => ShaderLayout = { case layout: Product =>
      layout.productIterator.zipWithIndex.map { case (binding: GBinding[?], i) => Binding(binding, ReadWrite) }.toSeq.pipe(Seq(_))
    }
    new SpirvProgram[Params, L]((il: InitProgramLayout) => layout(using il), dispatch, workgroupSize, code, main, f)
