package io.computenode.cyfra.vulkan.compute

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*

import scala.util.Using

/** @author
  *   MarconZet Created 14.04.2020
  */
private[cyfra] class ComputePipeline(val computeShader: Shader, context: VulkanContext) extends VulkanObjectHandle {
  private val device: Device = context.device
  val descriptorSetLayouts: Seq[(Long, LayoutSet)] =
    computeShader.layoutInfo.sets.map(x => (createDescriptorSetLayout(x), x))
  val pipelineLayout: Long = pushStack { stack =>
    val pipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .pSetLayouts(stack.longs(descriptorSetLayouts.map(_._1): _*))
      .pPushConstantRanges(null)

    val pPipelineLayout = stack.callocLong(1)
    check(vkCreatePipelineLayout(device.get, pipelineLayoutCreateInfo, null, pPipelineLayout), "Failed to create pipeline layout")
    pPipelineLayout.get(0)
  }
  protected val handle: Long = pushStack { stack =>
    val pipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .stage(VK_SHADER_STAGE_COMPUTE_BIT)
      .module(computeShader.get)
      .pName(stack.ASCII(computeShader.functionName))

    val computePipelineCreateInfo = VkComputePipelineCreateInfo.calloc(1, stack)
    computePipelineCreateInfo
      .get(0)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .stage(pipelineShaderStageCreateInfo)
      .layout(pipelineLayout)
      .basePipelineHandle(0)
      .basePipelineIndex(0)

    val pPipeline = stack.callocLong(1)
    check(vkCreateComputePipelines(device.get, 0, computePipelineCreateInfo, null, pPipeline), "Failed to create compute pipeline")
    pPipeline.get(0)
  }

  protected def close(): Unit = {
    vkDestroyPipeline(device.get, handle, null)
    vkDestroyPipelineLayout(device.get, pipelineLayout, null)
    descriptorSetLayouts.map(_._1).foreach(vkDestroyDescriptorSetLayout(device.get, _, null))
  }

  private def createDescriptorSetLayout(set: LayoutSet): Long = pushStack { stack =>
    val descriptorSetLayoutBindings = VkDescriptorSetLayoutBinding.calloc(set.bindings.length, stack)
    set.bindings.foreach { binding =>
      descriptorSetLayoutBindings
        .get()
        .binding(binding.id)
        .descriptorType(binding.size match
          case InputBufferSize(_) => VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
          case UniformSize(_) => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        )
        .descriptorCount(1)
        .stageFlags(VK_SHADER_STAGE_COMPUTE_BIT)
        .pImmutableSamplers(null)
    }
    descriptorSetLayoutBindings.flip()

    val descriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .pBindings(descriptorSetLayoutBindings)

    val pDescriptorSetLayout = stack.callocLong(1)
    check(vkCreateDescriptorSetLayout(device.get, descriptorSetLayoutCreateInfo, null, pDescriptorSetLayout), "Failed to create descriptor set layout")
    pDescriptorSetLayout.get(0)
  }
}
