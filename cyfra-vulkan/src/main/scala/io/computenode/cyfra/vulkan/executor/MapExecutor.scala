package io.computenode.cyfra.vulkan.executor

import io.computenode.cyfra.vulkan.compute.*
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.compute.{Binding, ComputePipeline, InputBufferSize, Shader, UniformSize}
import io.computenode.cyfra.vulkan.memory.{Buffer, DescriptorSet}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.util.vma.Vma.*
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*

import scala.collection.mutable
import scala.util.Using

/** @author
  *   MarconZet Created 15.04.2020
  */
private[cyfra] class MapExecutor(dataLength: Int, bufferActions: Seq[BufferAction], computePipeline: ComputePipeline, context: VulkanContext)
    extends AbstractExecutor(dataLength, bufferActions, context) {
  private lazy val shader: Shader = computePipeline.computeShader

  protected def getBiggestTransportData: Int = shader.layoutInfo.sets
    .flatMap(_.bindings)
    .collect { case Binding(_, InputBufferSize(n)) =>
      n
    }
    .max

  protected def setupBuffers(): (Seq[DescriptorSet], Seq[Buffer]) = pushStack { stack =>
    val bindings = shader.layoutInfo.sets.flatMap(_.bindings)
    val buffers = bindings.zipWithIndex.map { case (binding, i) =>
      val bufferSize = binding.size match {
        case InputBufferSize(n) => n * dataLength
        case UniformSize(n)     => n
      }
      new Buffer(bufferSize, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | bufferActions(i).action, 0, VMA_MEMORY_USAGE_GPU_ONLY, allocator)
    }

    val bufferDeque = mutable.ArrayDeque.from(buffers)
    val descriptorSetLayouts = computePipeline.descriptorSetLayouts
    val descriptorSets = for (i <- descriptorSetLayouts.indices) yield {
      val descriptorSet = new DescriptorSet(device, descriptorSetLayouts(i)._1, descriptorSetLayouts(i)._2.bindings, descriptorPool)
      val size = descriptorSetLayouts(i)._2.bindings.size
      descriptorSet.update(bufferDeque.take(size).toSeq)
      bufferDeque.drop(size)
      descriptorSet
    }
    (descriptorSets, buffers)
  }

  protected def recordCommandBuffer(commandBuffer: VkCommandBuffer): Unit =
    pushStack { stack =>
      vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, computePipeline.get)

      val pDescriptorSets = stack.longs(descriptorSets.map(_.get): _*)
      vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, computePipeline.pipelineLayout, 0, pDescriptorSets, null)

      val workgroup = shader.workgroupDimensions
      vkCmdDispatch(commandBuffer, dataLength / workgroup.x(), 1 / workgroup.y(), 1 / workgroup.z())
    }

}
