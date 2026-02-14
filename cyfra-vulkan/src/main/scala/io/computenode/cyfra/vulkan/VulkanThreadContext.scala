package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.memory.{DescriptorPoolManager, DescriptorSetManager}

/** Thread-local context holding command pool and descriptor set manager.
  */
case class VulkanThreadContext(
  commandPool: CommandPool.Reset,
  descriptorSetManager: DescriptorSetManager,
)

object VulkanThreadContext:
  val guard: ThreadLocal[Int] = new ThreadLocal[Int]:
    override def initialValue(): Int = 0
