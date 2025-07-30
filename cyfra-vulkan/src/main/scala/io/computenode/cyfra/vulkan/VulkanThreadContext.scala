package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.memory.{DescriptorPoolManager, DescriptorSetManager}

class VulkanThreadContext(val commandPool: CommandPool, poolManager: DescriptorPoolManager)(using Device) {
  val descriptorSetManager = new DescriptorSetManager(poolManager)
  
  def destroy(): Unit =
    descriptorSetManager.destroy()
}
