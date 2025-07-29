package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}
import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GExecution, GProgram, SpirvProgram}
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline

import scala.collection.mutable

class VkCyfraRuntime extends CyfraRuntime:
  private val context = new VulkanContext()
  import context.given

  private val shaderCache = mutable.Map.empty[String, VkShader[?]]
  private[cyfra] def getOrLoadProgram[Params, L <: Layout: {LayoutBinding, LayoutStruct}](program: GProgram[Params, L]): VkShader[L] =
    shaderCache.getOrElseUpdate(program.cacheKey, VkShader(program)).asInstanceOf[VkShader[L]]

  override def withAllocation(f: Allocation => Unit): Unit =
    context.withThreadContext: threadContext =>
      val executionHandler = new ExecutionHandler(this, threadContext, context)
      val allocation = new VkAllocation(threadContext.commandPool, executionHandler)
      f(allocation)
      allocation.close()

  def close(): Unit =
    shaderCache.values.foreach(_.underlying.destroy())
    context.destroy()
