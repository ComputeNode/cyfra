package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.compute.ComputePipeline.{BindingType, DescriptorSetInfo, DescriptorSetLayout}
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VK10, VK11, VkDescriptorBufferInfo, VkDescriptorSetAllocateInfo, VkWriteDescriptorSet}

/** @author
  *   MarconZet Created 15.04.2020
  */
private[cyfra] class DescriptorSet private (protected val handle: Long, val layout: DescriptorSetLayout)(using device: Device)
    extends VulkanObjectHandle:

  def update(buffers: Seq[Buffer]): Unit = pushStack: stack =>
    val bindings = layout.set.descriptors
    assert(buffers.length == bindings.length, s"Number of buffers (${buffers.length}) does not match number of bindings (${bindings.length})")
    val writeDescriptorSet = VkWriteDescriptorSet.calloc(buffers.length, stack)
    buffers
      .zip(bindings)
      .zipWithIndex
      .foreach:
        case ((buffer, binding), idx) =>
          val descriptorBufferInfo = VkDescriptorBufferInfo
            .calloc(1, stack)
            .buffer(buffer.get)
            .offset(0)
            .range(VK_WHOLE_SIZE)
          val descriptorType = binding.kind match
            case BindingType.StorageBuffer => VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
            case BindingType.Uniform       => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
          writeDescriptorSet
            .get()
            .sType$Default()
            .dstSet(handle)
            .dstBinding(idx)
            .descriptorCount(1)
            .descriptorType(descriptorType)
            .pBufferInfo(descriptorBufferInfo)
    writeDescriptorSet.rewind()
    vkUpdateDescriptorSets(device.get, writeDescriptorSet, null)

  override protected def close(): Unit = ()

object DescriptorSet:
  def apply(layout: DescriptorSetLayout, descriptorPool: DescriptorPool)(using device: Device): Option[DescriptorSet] =
    pushStack: stack =>
      val pSetLayout = stack.callocLong(1).put(0, layout.id)
      val descriptorSetAllocateInfo = VkDescriptorSetAllocateInfo
        .calloc(stack)
        .sType$Default()
        .descriptorPool(descriptorPool.get)
        .pSetLayouts(pSetLayout)

      val pDescriptorSet = stack.callocLong(1)
      val err = vkAllocateDescriptorSets(device.get, descriptorSetAllocateInfo, pDescriptorSet)
      if err == VK11.VK_ERROR_OUT_OF_POOL_MEMORY || err == VK10.VK_ERROR_FRAGMENTED_POOL then None
      else
        check(err, "Failed to allocate descriptor set")
        Some(new DescriptorSet(pDescriptorSet.get(), layout))
