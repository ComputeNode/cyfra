package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.core.{Device, Queue}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*

/** @author
  *   MarconZet Created 13.04.2020 Copied from Wrap
  */
private[cyfra] abstract class CommandPool private (flags: Int, val queue: Queue)(using device: Device) extends VulkanObjectHandle:
  protected val handle: Long = pushStack: stack =>
    val createInfo = VkCommandPoolCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .queueFamilyIndex(queue.familyIndex)
      .flags(flags)

    val pCommandPoll = stack.callocLong(1)
    check(vkCreateCommandPool(device.get, createInfo, null, pCommandPoll), "Failed to create command pool")
    pCommandPoll.get()

  def createCommandBuffer(): VkCommandBuffer =
    createCommandBuffers(1).head

  def createCommandBuffers(n: Int): Seq[VkCommandBuffer] = pushStack: stack =>
    val allocateInfo = VkCommandBufferAllocateInfo
      .calloc(stack)
      .sType$Default()
      .commandPool(handle)
      .level(VK_COMMAND_BUFFER_LEVEL_PRIMARY)
      .commandBufferCount(n)

    val pointerBuffer = stack.callocPointer(n)
    check(vkAllocateCommandBuffers(device.get, allocateInfo, pointerBuffer), "Failed to allocate command buffers")
    0 until n map (i => pointerBuffer.get(i)) map (new VkCommandBuffer(_, device.get))

  def recordSingleTimeCommand(block: VkCommandBuffer => Unit): VkCommandBuffer = pushStack: stack =>
    val commandBuffer = createCommandBuffer()

    val beginInfo = VkCommandBufferBeginInfo
      .calloc(stack)
      .sType$Default()
      .flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)

    check(vkBeginCommandBuffer(commandBuffer, beginInfo), "Failed to begin single time command buffer")
    block(commandBuffer)
    check(vkEndCommandBuffer(commandBuffer), "Failed to end single time command buffer")
    commandBuffer

  def reset(): Unit =
    check(vkResetCommandPool(device.get, handle, 0), "Failed to reset command pool")

  protected def close(): Unit =
    vkDestroyCommandPool(device.get, handle, null)

object CommandPool:
  private[cyfra] class Reset(queue: Queue, transient: Boolean = true)(using device: Device)
      extends CommandPool((if transient then VK_COMMAND_POOL_CREATE_TRANSIENT_BIT else 0) | VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, queue)(
        using device: Device,
      ):
    def freeCommandBuffer(commandBuffer: VkCommandBuffer*): Unit =
      pushStack: stack =>
        val pointerBuffer = stack.callocPointer(commandBuffer.length)
        commandBuffer.foreach(pointerBuffer.put)
        pointerBuffer.flip()
        vkFreeCommandBuffers(device.get, handle, pointerBuffer)

  private[cyfra] class Standard(queue: Queue, transient: Boolean = false)(using device: Device)
      extends CommandPool(if transient then VK_COMMAND_POOL_CREATE_TRANSIENT_BIT else 0, queue)(using device: Device)
