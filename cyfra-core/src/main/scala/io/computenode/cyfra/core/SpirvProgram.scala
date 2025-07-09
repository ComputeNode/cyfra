package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.layout.LayoutStruct
import io.computenode.cyfra.core.GProgram.{GioProgram, InitProgramLayout, ProgramDispatch, WorkDimensions}
import io.computenode.cyfra.core.SpirvProgram.Operation.ReadWrite
import io.computenode.cyfra.core.SpirvProgram.{Binding, ShaderLayout}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.GBinding
import izumi.reflect.Tag

import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.Objects
import scala.util.Try
import scala.util.Using
import scala.util.chaining.*

case class SpirvProgram[Params, L <: Layout: LayoutStruct](
  layout: InitProgramLayout => Params => L,
  dispatch: (L, Params) => ProgramDispatch,
  workgroupSize: WorkDimensions,
  code: ByteBuffer,
  entryPoint: String,
  shaderBindings: L => ShaderLayout,
) extends GProgram[Params, L]:
  private[cyfra] def cacheKey: String = toString

object SpirvProgram:
  type ShaderLayout = Seq[Seq[Binding]]
  case class Binding(binding: GBinding[?], operation: Operation)
  enum Operation:
    case Read
    case Write
    case ReadWrite

  def apply[Params, L <: Layout: LayoutStruct](
    path: String,
    layout: InitProgramLayout => Params => L,
    dispatch: (L, Params) => ProgramDispatch,
  ): SpirvProgram[Params, L] =
    val code = loadShader(path).get
    val workgroupSize = (128, 1, 1) // TODO Extract form shader
    val main = "main"
    val f: L => ShaderLayout = { case layout: Product =>
      layout.productIterator.zipWithIndex.map { case (binding: GBinding[?], i) => Binding(binding, ReadWrite) }.toSeq.pipe(Seq(_))
    }
    new SpirvProgram[Params, L](layout, dispatch, workgroupSize, code, main, f)

  private def loadShader(path: String, classLoader: ClassLoader = getClass.getClassLoader): Try[ByteBuffer] =
    Using.Manager: use =>
      val file = new File(Objects.requireNonNull(classLoader.getResource(path)).getFile)
      val fis = use(new FileInputStream(file))
      val fc = use(fis.getChannel)
      fc.map(FileChannel.MapMode.READ_ONLY, 0, fc.size())
