package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GExecution, GProgram, SpirvProgram}
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.ComputePipeline

import scala.collection.mutable

class VkCyfraRuntime extends CyfraRuntime:
  val context = new VulkanContext()
  import context.given

  private val executionHandler = new ExecutionHandler(this)

  private val shaderCache = mutable.Map.empty[String, VkShader[?]]
  private[cyfra] def getOrLoadProgram[Params, L <: Layout: LayoutStruct](program: GProgram[Params, L]): VkShader[L] =
    shaderCache.getOrElseUpdate(program.cacheKey, VkShader(program)).asInstanceOf[VkShader[L]]

  override def withAllocation(f: Allocation => Unit): Unit =
    val allocation = new VkAllocation(context.commandPool, executionHandler)
    f(allocation)
    allocation.close()
