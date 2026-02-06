package io.computenode.cyfra.runtime

import io.computenode.cyfra.compiler.Compiler
import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.{Allocation, CyfraRuntime, ExpressionProgram, GExecution, GProgram, Layout}
import io.computenode.cyfra.core.expression.Expression
import io.computenode.cyfra.core.memory.BindingRef
import io.computenode.cyfra.spirvtools.SpirvToolsRunner
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline

import java.nio.channels.FileChannel
import java.nio.file.{Paths, StandardOpenOption}
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.collection.mutable

class VkCyfraRuntime(spirvToolsRunner: SpirvToolsRunner = SpirvToolsRunner()) extends CyfraRuntime:
  private val context = new VulkanContext()
  import context.given

  private val gProgramCache = mutable.Map[GProgram[?, ?], SpirvProgram[?, ?]]()
  private val shaderCache = mutable.Map[(Long, Long), VkShader[?]]()
  private val compiler = new Compiler()

  private[cyfra] def getOrLoadProgram[Params, L: Layout](program: GProgram[Params, L]): VkShader[L] = synchronized:

    val spirvProgram: SpirvProgram[Params, L] = program match
      case p: ExpressionProgram[Params, L] if gProgramCache.contains(p) =>
        gProgramCache(p).asInstanceOf[SpirvProgram[Params, L]]
      case p: ExpressionProgram[Params, L] => compile(p)
      case p: SpirvProgram[Params, L]      => p
      case _                               => throw new IllegalArgumentException(s"Unsupported program type: ${program.getClass.getName}")

    gProgramCache.update(program, spirvProgram)
    shaderCache.getOrElseUpdate(spirvProgram.shaderHash, VkShader(spirvProgram, program.name)).asInstanceOf[VkShader[L]]

  private def compile[Params, L: Layout](program: ExpressionProgram[Params, L]): SpirvProgram[Params, L] =
    val ExpressionProgram(body, layout, dispatch, workgroupSize, programName) = program
    val bindings = Layout[L].toBindings(Layout[L].layoutRef).toList
    val config = Compiler.Compute(bindings, workgroupSize)
    val expressions = body(Layout[L].layoutRef)
    val compiled = compiler.compile(expressions, config)
    val optimizedShaderCode = spirvToolsRunner.processShaderCodeWithSpirvTools(compiled)
    // Extract written binding indices for smarter barrier insertion
    val writtenBindingIndices: Set[Int] = VkCyfraRuntime.getWrittenBindingIndices(expressions.body, Set.empty)
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
  private[runtime] def getWrittenBindingIndices(pending: List[Expression[?]], acc: Set[Int]): Set[Int] =
    pending match
      case Nil => acc
      case Expression.Write(buffer: BindingRef[?], _, _) :: tail =>
        getWrittenBindingIndices(tail, acc + buffer.layoutOffset)
      case Expression.Branch(_, ifTrue, ifFalse, _) :: tail =>
        getWrittenBindingIndices(ifTrue.body ::: ifFalse.body ::: tail, acc)
      case Expression.Loop(body, continueBody, _, _) :: tail =>
        getWrittenBindingIndices(body.body ::: continueBody.body ::: tail, acc)
      case Expression.CustomCall(function, _) :: tail =>
        getWrittenBindingIndices(function.body.body ::: tail, acc)
      case _ :: tail => getWrittenBindingIndices(tail, acc)
