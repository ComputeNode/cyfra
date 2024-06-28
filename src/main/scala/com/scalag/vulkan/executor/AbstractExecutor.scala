package com.scalag.vulkan.executor

import com.scalag.vulkan.VulkanContext
import com.scalag.vulkan.command.{CommandPool, Fence, Queue}
import com.scalag.vulkan.core.Device
import com.scalag.vulkan.memory.{Allocator, Buffer, DescriptorPool, DescriptorSet}
import com.scalag.vulkan.util.Util.{check, pushStack}
import com.scalag.vulkan.util.VulkanAssertionError
import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.util.vma.Vma.VMA_MEMORY_USAGE_UNKNOWN
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*

import java.nio.ByteBuffer
import scala.util.Using

abstract class AbstractExecutor(dataLength: Int, val bufferActions: Seq[BufferAction], context: VulkanContext) {
  protected val device: Device = context.device
  protected val queue: Queue = context.computeQueue
  protected val allocator: Allocator = context.allocator
  protected val descriptorPool: DescriptorPool = context.descriptorPool
  protected val commandPool: CommandPool = context.commandPool

  protected val (descriptorSets, buffers) = setupBuffers()
  private val commandBuffer: VkCommandBuffer =
    pushStack { stack =>
      val commandBuffer = commandPool.createCommandBuffer()

      val commandBufferBeginInfo = VkCommandBufferBeginInfo
        .calloc(stack)
        .sType$Default()
        .flags(0)

      check(vkBeginCommandBuffer(commandBuffer, commandBufferBeginInfo), "Failed to begin recording command buffer")

      recordCommandBuffer(commandBuffer)

      check(vkEndCommandBuffer(commandBuffer), "Failed to finish recording command buffer")
      commandBuffer
    }

  def execute(input: Seq[ByteBuffer]): Seq[ByteBuffer] = {
    val stagingBuffer = new Buffer(
      getBiggestTransportData * dataLength,
      VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
      VMA_MEMORY_USAGE_UNKNOWN,
      allocator
    )
    for (i <- bufferActions.indices if bufferActions(i) == BufferAction.LOAD_INTO) do {
      val buffer = input(i)
      Buffer.copyBuffer(buffer, stagingBuffer, buffer.remaining())
      Buffer.copyBuffer(stagingBuffer, buffers(i), buffer.remaining(), commandPool).block().destroy()
    }

    pushStack { stack =>
      val fence = new Fence(device)
      val pCommandBuffer = stack.callocPointer(1).put(0, commandBuffer)
      val submitInfo = VkSubmitInfo
        .calloc(stack)
        .sType$Default()
        .pCommandBuffers(pCommandBuffer)

      check(VK10.vkQueueSubmit(queue.get, submitInfo, fence.get), "Failed to submit command buffer to queue")
      fence.block().destroy()
    }

    val output = for (i <- bufferActions.indices if bufferActions(i) == BufferAction.LOAD_FROM) yield {
      val fence = Buffer.copyBuffer(buffers(i), stagingBuffer, buffers(i).size, commandPool)
      val outBuffer = BufferUtils.createByteBuffer(buffers(i).size)
      fence.block().destroy()
      Buffer.copyBuffer(stagingBuffer, outBuffer, outBuffer.remaining())
      outBuffer

    }
    stagingBuffer.destroy()
    output
  }

  def destroy(): Unit = {
    commandPool.freeCommandBuffer(commandBuffer)
    descriptorSets.foreach(_.destroy())
    buffers.foreach(_.destroy())
  }

  protected def setupBuffers(): (Seq[DescriptorSet], Seq[Buffer])

  protected def recordCommandBuffer(commandBuffer: VkCommandBuffer): Unit

  protected def getBiggestTransportData: Int

  protected def createUpdatedDescriptorSet(descriptorSetLayout: Long, buffers: Seq[Buffer]): DescriptorSet = pushStack { stack =>
    val descriptorSet = new DescriptorSet(device, descriptorSetLayout, descriptorPool)
    val writeDescriptorSet = VkWriteDescriptorSet.calloc(buffers.length, stack)
    buffers.indices foreach { i =>
      val descriptorBufferInfo = VkDescriptorBufferInfo
        .calloc(1, stack)
        .buffer(buffers(i).get)
        .offset(0)
        .range(VK_WHOLE_SIZE)

      writeDescriptorSet
        .get(i)
        .sType$Default()
        .dstSet(descriptorSet.get)
        .dstBinding(i)
        .descriptorCount(1)
        .descriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
        .pBufferInfo(descriptorBufferInfo)
    }
    vkUpdateDescriptorSets(device.get, writeDescriptorSet, null)
    descriptorSet
  }
}
