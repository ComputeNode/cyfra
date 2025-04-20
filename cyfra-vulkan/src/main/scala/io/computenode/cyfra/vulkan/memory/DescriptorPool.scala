package io.computenode.cyfra.vulkan.memory

import DescriptorPool.MAX_SETS
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkDescriptorPoolCreateInfo, VkDescriptorPoolSize}

import java.nio.LongBuffer
import scala.util.Using

/** @author
  *   MarconZet Created 14.04.2019
  */
object DescriptorPool {
  val MAX_SETS = 100
}
private[cyfra] class DescriptorPool(device: Device) extends VulkanObjectHandle {
  protected val handle: Long = pushStack { stack =>
    val descriptorPoolSize = VkDescriptorPoolSize.calloc(1, stack)
    descriptorPoolSize
      .get(0)
      .`type`(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
      .descriptorCount(2 * MAX_SETS)

    val descriptorPoolCreateInfo = VkDescriptorPoolCreateInfo
      .calloc(stack)
      .sType$Default()
      .maxSets(MAX_SETS)
      .flags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT)
      .pPoolSizes(descriptorPoolSize)

    val pDescriptorPool = stack.callocLong(1)
    check(vkCreateDescriptorPool(device.get, descriptorPoolCreateInfo, null, pDescriptorPool), "Failed to create descriptor pool")
    pDescriptorPool.get()
  }

  override protected def close(): Unit =
    vkDestroyDescriptorPool(device.get, handle, null)
}
