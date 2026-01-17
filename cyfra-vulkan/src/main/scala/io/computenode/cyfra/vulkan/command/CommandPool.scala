package io.computenode.cyfra.vulkan.command

import io.computenode.cyfra.vulkan.core.{Device, Queue}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*

import scala.collection.mutable
import scala.util.boundary.break

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

  private val allBuffers: mutable.Set[VkCommandBuffer] = mutable.Set.empty
  protected val reclaimedBuffers: mutable.Queue[VkCommandBuffer] = mutable.Queue.empty

  def createCommandBuffer(): VkCommandBuffer =
    reclaimedBuffers.removeHeadOption().getOrElse(createNewCommandBuffers(1).head)

  private def createNewCommandBuffers(n: Int): Seq[VkCommandBuffer] = pushStack: stack =>
    if n == 0 then break(Seq.empty)

    val allocateInfo = VkCommandBufferAllocateInfo
      .calloc(stack)
      .sType$Default()
      .commandPool(handle)
      .level(VK_COMMAND_BUFFER_LEVEL_PRIMARY)
      .commandBufferCount(n)

    val pointerBuffer = stack.callocPointer(n)
    check(vkAllocateCommandBuffers(device.get, allocateInfo, pointerBuffer), "Failed to allocate command buffers")
    val res = 0 until n map (i => pointerBuffer.get(i)) map (new VkCommandBuffer(_, device.get))
    allBuffers.addAll(res)
    res

  def freeCommandBuffer(commandBuffers: VkCommandBuffer*): Unit =
    pushStack: stack =>
      val pointerBuffer = stack.callocPointer(commandBuffers.length)
      commandBuffers.foreach(pointerBuffer.put)
      pointerBuffer.flip()
      vkFreeCommandBuffers(device.get, handle, pointerBuffer)
      commandBuffers.foreach(allBuffers.remove)

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
    reclaimedBuffers.removeAll()
    reclaimedBuffers.enqueueAll(allBuffers)

  protected def close(): Unit =
    vkDestroyCommandPool(device.get, handle, null)

object CommandPool:
  private[cyfra] class Reset(queue: Queue, transient: Boolean = true)(using device: Device)
      extends CommandPool((if transient then VK_COMMAND_POOL_CREATE_TRANSIENT_BIT else 0) | VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, queue)(
        using device: Device,
      ):
    def resetCommandBuffer(commandBuffer: VkCommandBuffer): Unit =
      check(vkResetCommandBuffer(commandBuffer, 0), "Failed to reset command buffer")
      reclaimedBuffers.addOne(commandBuffer)

  private[cyfra] class Standard(queue: Queue, transient: Boolean = false)(using device: Device)
      extends CommandPool(if transient then VK_COMMAND_POOL_CREATE_TRANSIENT_BIT else 0, queue)(using device: Device)
