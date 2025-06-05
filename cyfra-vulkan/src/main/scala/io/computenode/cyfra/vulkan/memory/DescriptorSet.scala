package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.compute.{Binding, LayoutSet}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkDescriptorBufferInfo, VkDescriptorSetAllocateInfo, VkWriteDescriptorSet}

/** @author
  *   MarconZet Created 15.04.2020
  */
private[cyfra] class DescriptorSet(device: Device, descriptorSetLayout: Long, val bindings: Seq[Binding], descriptorPool: DescriptorPool) extends VulkanObjectHandle {

  protected val handle: Long = pushStack { stack =>
    val pSetLayout = stack.callocLong(1).put(0, descriptorSetLayout)
    val descriptorSetAllocateInfo = VkDescriptorSetAllocateInfo
      .calloc(stack)
      .sType$Default()
      .descriptorPool(descriptorPool.get)
      .pSetLayouts(pSetLayout)

    val pDescriptorSet = stack.callocLong(1)
    check(vkAllocateDescriptorSets(device.get, descriptorSetAllocateInfo, pDescriptorSet), "Failed to allocate descriptor set")
    pDescriptorSet.get()
  }
  
  def update(buffers: Seq[Buffer]): Unit = pushStack { stack =>
    val writeDescriptorSet = VkWriteDescriptorSet.calloc(buffers.length, stack)
    buffers.indices foreach { i =>
      val descriptorBufferInfo = VkDescriptorBufferInfo
        .calloc(1, stack)
        .buffer(buffers(i).get)
        .offset(0)
        .range(VK_WHOLE_SIZE)
      val descriptorType = buffers(i).usage match
        case storage if (storage & VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) != 0 => VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
        case uniform if (uniform & VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT) != 0 => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
      writeDescriptorSet
        .get(i)
        .sType$Default()
        .dstSet(handle)
        .dstBinding(i)
        .descriptorCount(1)
        .descriptorType(descriptorType)
        .pBufferInfo(descriptorBufferInfo)
    }
    vkUpdateDescriptorSets(device.get, writeDescriptorSet, null)
  }

  override protected def close(): Unit =
    vkFreeDescriptorSets(device.get, descriptorPool.get, handle)
}
