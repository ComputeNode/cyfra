package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.compute.ComputePipeline.DescriptorSetLayout
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.memory.DescriptorPool.MAX_SETS
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkDescriptorPoolCreateInfo, VkDescriptorPoolSize}

/** @author
  *   MarconZet Created 14.04.2019
  */
object DescriptorPool:
  val MAX_SETS = 1000
private[cyfra] class DescriptorPool(using device: Device) extends VulkanObjectHandle:
  protected val handle: Long = pushStack: stack =>
    val descriptorPoolSize = VkDescriptorPoolSize.calloc(2, stack)
    descriptorPoolSize
      .get()
      .`type`(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
      .descriptorCount(10 * MAX_SETS)

    descriptorPoolSize
      .get()
      .`type`(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
      .descriptorCount(2 * MAX_SETS)
    descriptorPoolSize.rewind()

    val descriptorPoolCreateInfo = VkDescriptorPoolCreateInfo
      .calloc(stack)
      .sType$Default()
      .maxSets(MAX_SETS)
      .pPoolSizes(descriptorPoolSize)

    val pDescriptorPool = stack.callocLong(1)
    check(vkCreateDescriptorPool(device.get, descriptorPoolCreateInfo, null, pDescriptorPool), "Failed to create descriptor pool")
    pDescriptorPool.get()

  def allocate(layout: DescriptorSetLayout): Option[DescriptorSet] = DescriptorSet(layout, this)
  
  def reset(): Unit = check(vkResetDescriptorPool(device.get, handle, 0), "Failed to reset descriptor pool")

  override protected def close(): Unit =
    vkDestroyDescriptorPool(device.get, handle, null)
