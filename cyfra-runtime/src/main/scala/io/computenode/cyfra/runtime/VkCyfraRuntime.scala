package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.{Allocation, CyfraRuntime}
import io.computenode.cyfra.vulkan.VulkanContext

class VkCyfraRuntime extends CyfraRuntime:
  private val context = new VulkanContext()
  import context.given

  override def withAllocation(f: Allocation => Unit): Unit =
    val allocation = new VkAllocation(context.commandPool)
    f(allocation)
    allocation.close()
