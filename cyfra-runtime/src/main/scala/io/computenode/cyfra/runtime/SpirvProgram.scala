package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.{GProgram, Layout}
import io.computenode.cyfra.core.GProgram.{InitProgramLayout, ProgramDispatch, WorkDimensions}
import io.computenode.cyfra.runtime.SpirvProgram.Operation.ReadWrite
import io.computenode.cyfra.runtime.SpirvProgram.{Binding, ShaderLayout}
import io.computenode.cyfra.core.memory.GBinding
import io.computenode.cyfra.core.expression.Value
import izumi.reflect.Tag

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.security.MessageDigest
import java.util.Objects
import scala.util.{Try, Using}
import scala.util.chaining.*

case class SpirvProgram[Params, L: Layout] private (
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
    val layout = shaderBindings(Layout[L].layoutRef)
    layout.flatten.foreach: binding =>
      md.update(binding.binding.v.tag.toString.getBytes)
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

  def fromFile[Params, L: Layout](
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

  def apply[Params, L: Layout](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    code: ByteBuffer,
  ): SpirvProgram[Params, L] =
    apply(layout, dispatch, code, Set.empty)

  /** Create a SpirvProgram with explicit write tracking for smarter barrier insertion.
    *
    * @param writtenBuffers Set of buffers that are written to by this shader.
    *                       Buffers not in this set are treated as read-only.
    */
  def apply[Params, L: Layout](
    layout: InitProgramLayout ?=> Params => L,
    dispatch: (L, Params) => ProgramDispatch,
    code: ByteBuffer,
    writtenBuffers: Set[GBinding[?]],
  ): SpirvProgram[Params, L] =
    val workgroupSize = (128, 1, 1) // TODO  Extract from shader
    val main = "main"
    val f: L => ShaderLayout = { case layout: Product =>
      layout.productIterator.zipWithIndex.map { case (binding: GBinding[?], i) =>
        val op = if writtenBuffers.isEmpty then ReadWrite  // Fallback for legacy code
                 else if writtenBuffers.contains(binding) then Write
                 else Read
        Binding(binding, op)
      }.toSeq.pipe(Seq(_))
    }
    new SpirvProgram[Params, L]((il: InitProgramLayout) => layout(using il), dispatch, workgroupSize, code, main, f)
