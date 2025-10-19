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

  private val commandPool = handle

  def createCommandBuffer(): VkCommandBuffer =
    createCommandBuffers(1).head

  def createCommandBuffers(n: Int): Seq[VkCommandBuffer] = pushStack: stack =>
    val allocateInfo = VkCommandBufferAllocateInfo
      .calloc(stack)
      .sType$Default()
      .commandPool(commandPool)
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

  def freeCommandBuffer(commandBuffer: VkCommandBuffer*): Unit =
    pushStack: stack =>
      val pointerBuffer = stack.callocPointer(commandBuffer.length)
      commandBuffer.foreach(pointerBuffer.put)
      pointerBuffer.flip()
      // TODO remove vkQueueWaitIdle, but currently crashes without it - Likely the printf debug buffer is still in use?
      vkQueueWaitIdle(queue.get)
      vkFreeCommandBuffers(device.get, commandPool, pointerBuffer)

  protected def close(): Unit =
    vkDestroyCommandPool(device.get, commandPool, null)

object CommandPool:
  private[cyfra] class Transient(queue: Queue)(using device: Device)
      extends CommandPool(VK_COMMAND_POOL_CREATE_TRANSIENT_BIT, queue)(using device: Device) // TODO check if flags should be used differently

  private[cyfra] class Standard(queue: Queue)(using device: Device) extends CommandPool(0, queue)(using device: Device)
