package io.computenode.cyfra.vulkan.compute

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import io.computenode.cyfra.vulkan.compute.ComputePipeline.*
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.util.Objects
import scala.util.{Try, Using}

/** @author
  *   MarconZet Created 14.04.2020
  */
private[cyfra] class ComputePipeline(shaderCode: ByteBuffer, functionName: String, layoutInfo: LayoutInfo)(using device: Device)
    extends VulkanObjectHandle:

  private val shader: Long = pushStack: stack => // TODO khr_maintenance5
    val shaderModuleCreateInfo = VkShaderModuleCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .pCode(shaderCode)

    val pShaderModule = stack.callocLong(1)
    check(vkCreateShaderModule(device.get, shaderModuleCreateInfo, null, pShaderModule), "Failed to create shader module")
    pShaderModule.get()

  val pipelineLayout: PipelineLayout = pushStack: stack =>
    val descriptorSetLayouts: Seq[DescriptorSetLayout] = layoutInfo.sets.map(x => DescriptorSetLayout(createDescriptorSetLayout(x), x))

    val pipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .pSetLayouts(stack.longs(descriptorSetLayouts.map(_._1)*))
      .pPushConstantRanges(null)

    val pPipelineLayout = stack.callocLong(1)
    check(vkCreatePipelineLayout(device.get, pipelineLayoutCreateInfo, null, pPipelineLayout), "Failed to create pipeline layout")
    val layout = pPipelineLayout.get(0)
    PipelineLayout(layout, descriptorSetLayouts)

  protected val handle: Long = pushStack: stack =>
    val pipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .stage(VK_SHADER_STAGE_COMPUTE_BIT)
      .module(shader)
      .pName(stack.ASCII(functionName))

    val computePipelineCreateInfo = VkComputePipelineCreateInfo.calloc(1, stack)
    computePipelineCreateInfo
      .get(0)
      .sType$Default()
      .pNext(0)
      .flags(0)
      .stage(pipelineShaderStageCreateInfo)
      .layout(pipelineLayout.id)
      .basePipelineHandle(0)
      .basePipelineIndex(0)

    val pPipeline = stack.callocLong(1)
    check(vkCreateComputePipelines(device.get, 0, computePipelineCreateInfo, null, pPipeline), "Failed to create compute pipeline") // TODO vkCreatePipelineCache
    pPipeline.get(0)

  protected def close(): Unit =
    vkDestroyPipeline(device.get, handle, null)
    vkDestroyPipelineLayout(device.get, pipelineLayout.id, null)
    pipelineLayout.sets.map(_.id).foreach(vkDestroyDescriptorSetLayout(device.get, _, null))
    vkDestroyShaderModule(device.get, handle, null)

  private def createDescriptorSetLayout(set: DescriptorSetInfo): Long = pushStack: stack =>
    val descriptorSetLayoutBindings = VkDescriptorSetLayoutBinding.calloc(set.descriptors.length, stack)
    set.descriptors.zipWithIndex.foreach: binding =>
      descriptorSetLayoutBindings
        .get()
        .binding(binding._2)
        .descriptorType(binding._1.kind match
          case BindingType.StorageBuffer => VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
          case BindingType.Uniform       => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
        .descriptorCount(1)
        .stageFlags(VK_SHADER_STAGE_COMPUTE_BIT)
        .pImmutableSamplers(null)

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

object ComputePipeline:
  def loadShader(path: String): Try[ByteBuffer] = ???

  private[cyfra] case class PipelineLayout(id: Long, sets: Seq[DescriptorSetLayout])
  private[cyfra] case class DescriptorSetLayout(id: Long, set: DescriptorSetInfo)

  private[cyfra] case class LayoutInfo(sets: Seq[DescriptorSetInfo])
  private[cyfra] case class DescriptorSetInfo(descriptors: Seq[DescriptorInfo])
  private[cyfra] case class DescriptorInfo(kind: BindingType)

  private[cyfra] enum BindingType:
    case StorageBuffer
    case Uniform
