package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GExecution, GProgram, GioProgram, SpirvProgram}
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirvtools.SpirvToolsRunner
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline

import java.security.MessageDigest
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
    shaderCache.getOrElseUpdate(spirvProgram.shaderHash, VkShader(spirvProgram)).asInstanceOf[VkShader[L]]

  private def compile[Params, L: Layout as l](program: GioProgram[Params, L]): SpirvProgram[Params, L] =
    val GioProgram(_, layout, dispatch, _) = program
    val bindings = l.toBindings(l.layoutRef).toList
    val compiled = DSLCompiler.compile(program.body(l.layoutRef), bindings)
    val optimizedShaderCode = spirvToolsRunner.processShaderCodeWithSpirvTools(compiled)
    SpirvProgram((il: InitProgramLayout) ?=> layout(il), dispatch, optimizedShaderCode)

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
  def using[T](f: VkCyfraRuntime ?=> T): T =
    val runtime = new VkCyfraRuntime()
    try f(using runtime)
    finally runtime.close()
