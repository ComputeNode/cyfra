package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*

import scala.util.Using

/** @author
  *   MarconZet Created 13.04.2020 Copied from Wrap
  */
private[cyfra] abstract class CommandPool(device: Device, queue: Queue) extends VulkanObjectHandle {
  protected val handle: Long = pushStack { stack =>
    val createInfo = VkCommandPoolCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(VK_NULL_HANDLE)
      .queueFamilyIndex(queue.familyIndex)
      .flags(getFlags)

    val pCommandPoll = stack.callocLong(1)
    check(vkCreateCommandPool(device.get, createInfo, null, pCommandPoll), "Failed to create command pool")
    pCommandPoll.get()
  }

  private val commandPool = handle

  def beginSingleTimeCommands(): VkCommandBuffer =
    pushStack { stack =>
      val commandBuffer = this.createCommandBuffer()

      val beginInfo = VkCommandBufferBeginInfo
        .calloc(stack)
        .sType$Default()
        .flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)

      check(vkBeginCommandBuffer(commandBuffer, beginInfo), "Failed to begin single time command buffer")
      commandBuffer
    }

  def createCommandBuffer(): VkCommandBuffer =
    createCommandBuffers(1).head

  def createCommandBuffers(n: Int): Seq[VkCommandBuffer] = pushStack { stack =>
    val allocateInfo = VkCommandBufferAllocateInfo
      .calloc(stack)
      .sType$Default()
      .commandPool(commandPool)
      .level(VK_COMMAND_BUFFER_LEVEL_PRIMARY)
      .commandBufferCount(n)

    val pointerBuffer = stack.callocPointer(n)
    check(vkAllocateCommandBuffers(device.get, allocateInfo, pointerBuffer), "Failed to allocate command buffers")
    0 until n map (i => pointerBuffer.get(i)) map (new VkCommandBuffer(_, device.get))
  }

  def endSingleTimeCommands(commandBuffer: VkCommandBuffer): Fence =
    pushStack { stack =>
      vkEndCommandBuffer(commandBuffer)

      val pointerBuffer = stack.callocPointer(1).put(0, commandBuffer)
      val submitInfo = VkSubmitInfo
        .calloc(stack)
        .sType$Default()
        .pCommandBuffers(pointerBuffer)

      val fence = new Fence(device, 0, () => freeCommandBuffer(commandBuffer))
      queue.submit(submitInfo, fence)
      fence
    }

  def freeCommandBuffer(commandBuffer: VkCommandBuffer*): Unit =
    pushStack { stack =>
      val pointerBuffer = stack.callocPointer(commandBuffer.length)
      commandBuffer.foreach(pointerBuffer.put)
      pointerBuffer.flip()
      vkFreeCommandBuffers(device.get, commandPool, pointerBuffer)
    }

  protected def close(): Unit =
    vkDestroyCommandPool(device.get, commandPool, null)

  protected def getFlags: Int
}
