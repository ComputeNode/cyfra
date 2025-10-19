package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.memory.{DescriptorPoolManager, DescriptorSetManager}

case class VulkanThreadContext(commandPool: CommandPool, descriptorSetManager: DescriptorSetManager)

object VulkanThreadContext:
  val guard: ThreadLocal[Int] = new ThreadLocal[Int]:
    override def initialValue(): Int = 0
