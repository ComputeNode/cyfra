package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.GProgram.InitProgramLayout
import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}
import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GExecution, GProgram, GioProgram, SpirvProgram}
import io.computenode.cyfra.spirv.compilers.DSLCompiler
import io.computenode.cyfra.spirvtools.SpirvToolsRunner
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline

import scala.collection.mutable

class VkCyfraRuntime(spirvToolsRunner: SpirvToolsRunner = SpirvToolsRunner()) extends CyfraRuntime:
  private val context = new VulkanContext()
  import context.given

  private val shaderCache = mutable.Map[GProgram[?, ?], VkShader[?]]()
  private[cyfra] def getOrLoadProgram[Params, L <: Layout: {LayoutBinding, LayoutStruct}](program: GProgram[Params, L]): VkShader[L] = synchronized:
    if (shaderCache.contains(program)) then
      shaderCache(program).asInstanceOf[VkShader[L]]
    else
      val spirvProgram = program match
        case p: GioProgram[?, ?]   => compile(p)
        case p: SpirvProgram[?, ?] => p
        case _                     => throw new IllegalArgumentException(s"Unsupported program type: ${program.getClass.getName}")

      shaderCache.getOrElseUpdate(program, VkShader(spirvProgram)).asInstanceOf[VkShader[L]]

  private def compile[Params, L <: Layout: {LayoutBinding as lbinding, LayoutStruct as lstruct}](program: GioProgram[Params, L]): SpirvProgram[Params, L] =
    val GioProgram(_, layout, dispatch, _) = program
    val bindings = lbinding.toBindings(lstruct.layoutRef).toList
    val compiled = DSLCompiler.compile(program.body(summon[LayoutStruct[L]].layoutRef), bindings)
    val optimizedShaderCode = spirvToolsRunner.processShaderCodeWithSpirvTools(compiled)
    SpirvProgram((il: InitProgramLayout) ?=> layout(il), dispatch, optimizedShaderCode)

  override def withAllocation(f: Allocation => Unit): Unit =
    context.withThreadContext: threadContext =>
      val executionHandler = new ExecutionHandler(this, threadContext, context)
      val allocation = new VkAllocation(threadContext.commandPool, executionHandler)
      f(allocation)
      allocation.close()

  def close(): Unit =
    shaderCache.values.foreach(_.underlying.destroy())
    context.destroy()
