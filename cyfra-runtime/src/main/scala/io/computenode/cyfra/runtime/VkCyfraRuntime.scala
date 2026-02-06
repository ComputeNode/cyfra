package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.binding.BufferRef
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GExecution, GProgram, GioProgram, SpirvProgram}
import io.computenode.cyfra.dsl.binding.{WriteBuffer, WriteShared, WriteUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirvtools.SpirvToolsRunner
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline

import java.security.MessageDigest
import scala.annotation.tailrec
import scala.collection.mutable

class VkCyfraRuntime(spirvToolsRunner: SpirvToolsRunner = SpirvToolsRunner()) extends CyfraRuntime:
  private val context = new VulkanContext()
  import context.given

  private val gProgramCache = mutable.Map[GProgram[?, ?], SpirvProgram[?, ?]]()
  private val shaderCache = mutable.Map[(Long, Long), VkShader[?]]()

  private[cyfra] def getOrLoadProgram[Params, L: Layout](program: GProgram[Params, L]): VkShader[L] = synchronized:

    val spirvProgram: SpirvProgram[Params, L] = program match
      case p: GioProgram[Params, L] if gProgramCache.contains(p) =>
        gProgramCache(p).asInstanceOf[SpirvProgram[Params, L]]
      case p: GioProgram[Params, L]   => compile(p)
      case p: SpirvProgram[Params, L] => p
      case _                          => throw new IllegalArgumentException(s"Unsupported program type: ${program.getClass.getName}")

    gProgramCache.update(program, spirvProgram)
    shaderCache.getOrElseUpdate(spirvProgram.shaderHash, VkShader(spirvProgram, program.name)).asInstanceOf[VkShader[L]]

  private def compile[Params, L: Layout as l](program: GioProgram[Params, L]): SpirvProgram[Params, L] =
    val GioProgram(_, layout, dispatch, workgroupSize, programName) = program
    val bindings = l.toBindings(l.layoutRef).toList
    val bodyGio = program.body(l.layoutRef)
    val compiled = DSLCompiler.compile(bodyGio, bindings, workgroupSize)
    val optimizedShaderCode = spirvToolsRunner.processShaderCodeWithSpirvTools(compiled)
    // Extract written binding indices for smarter barrier insertion
    val writtenBindingIndices: Set[Int] = VkCyfraRuntime.getWrittenBindingIndices(List(bodyGio), Set.empty)
    SpirvProgram((il: InitProgramLayout) ?=> layout(il), dispatch, optimizedShaderCode, writtenBindingIndices, programName)

  override def withAllocation(f: Allocation => Unit): Unit =
    context.withThreadContext: threadContext =>
      val executionHandler = new ExecutionHandler(this, threadContext, context)
      val allocation = new VkAllocation(threadContext.commandPool, executionHandler)
      try f(allocation)
      finally allocation.close()

  def close(): Unit =
    shaderCache.values.foreach(_.underlying.destroy())
    context.destroy()

object VkCyfraRuntime:
  def using[T](f: VkCyfraRuntime ?=> T)(using spirvTools: SpirvToolsRunner = SpirvToolsRunner()): T =
    val runtime = new VkCyfraRuntime(spirvTools)
    try f(using runtime)
    finally runtime.close()

  /** Extract binding indices of all GBuffers that are written to in the GIO program.
    * Used for smarter barrier insertion - only written buffers cause conflicts.
    * Returns Set of layoutOffset values (binding indices).
    */
  @tailrec
  private[runtime] def getWrittenBindingIndices(pending: List[GIO[?]], acc: Set[Int]): Set[Int] =
    pending match
      case Nil => acc
      case GIO.Pure(_) :: tail =>
        getWrittenBindingIndices(tail, acc)
      case GIO.FlatMap(v, n) :: tail =>
        getWrittenBindingIndices(v :: n :: tail, acc)
      case GIO.Repeat(_, gio, _) :: tail =>
        getWrittenBindingIndices(gio :: tail, acc)
      case GIO.FoldRepeat(_, _, gio, _, _) :: tail =>
        getWrittenBindingIndices(gio :: tail, acc)
      case WriteBuffer(buffer: BufferRef[?], _, _) :: tail =>
        getWrittenBindingIndices(tail, acc + buffer.layoutOffset)
      case WriteBuffer(_, _, _) :: tail =>
        getWrittenBindingIndices(tail, acc) // Non-BufferRef buffer, can't track
      case WriteShared(_, _, _) :: tail =>
        getWrittenBindingIndices(tail, acc) // GShared is workgroup-local, not relevant for dispatch barriers
      case WriteUniform(_, _) :: tail =>
        getWrittenBindingIndices(tail, acc) // Uniforms are typically read-only from GPU perspective
      case GIO.Printf(_, _*) :: tail =>
        getWrittenBindingIndices(tail, acc)
      case GIO.WorkgroupBarrier :: tail =>
        getWrittenBindingIndices(tail, acc)
