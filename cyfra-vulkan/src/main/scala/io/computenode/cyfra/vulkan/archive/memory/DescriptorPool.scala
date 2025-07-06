package io.computenode.cyfra.vulkan.archive.memory

import io.computenode.cyfra.vulkan.archive.core.Device
import io.computenode.cyfra.vulkan.archive.memory.DescriptorPool.MAX_SETS
import io.computenode.cyfra.vulkan.archive.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.archive.util.VulkanObjectHandle
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkDescriptorPoolCreateInfo, VkDescriptorPoolSize}

/** @author
  *   MarconZet Created 14.04.2019
  */
object DescriptorPool:
  val MAX_SETS = 100
private[cyfra] class DescriptorPool(device: Device) extends VulkanObjectHandle:
  protected val handle: Long = pushStack: stack =>
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

  override protected def close(): Unit =
    vkDestroyDescriptorPool(device.get, handle, null)
