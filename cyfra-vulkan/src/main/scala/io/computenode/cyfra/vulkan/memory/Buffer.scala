package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.util.vma.Vma.*
import org.lwjgl.util.vma.VmaAllocationCreateInfo
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkBufferCopy, VkBufferCreateInfo, VkCommandBuffer, VkSubmitInfo}

import java.nio.ByteBuffer

/** @author
  *   MarconZet Created 11.05.2019
  */

private[cyfra] sealed abstract class Buffer private (val size: Int, usage: Int, flags: Int)(using allocator: Allocator) extends VulkanObjectHandle:
  val (handle, allocation) = pushStack: stack =>
    val bufferInfo = VkBufferCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(0)
      .size(size)
      .usage(usage)
      .flags(0)
      .sharingMode(VK_SHARING_MODE_EXCLUSIVE)

    val allocInfo = VmaAllocationCreateInfo
      .calloc(stack)
      .usage(VMA_MEMORY_USAGE_UNKNOWN)
      .requiredFlags(flags)

    val pBuffer = stack.callocLong(1)
    val pAllocation = stack.callocPointer(1)
    check(vmaCreateBuffer(allocator.get, bufferInfo, allocInfo, pBuffer, pAllocation, null), "Failed to create buffer")
    (pBuffer.get(), pAllocation.get())

  protected def close(): Unit =
    vmaDestroyBuffer(allocator.get, handle, allocation)

object Buffer:
  private[cyfra] class DeviceBuffer(size: Int, usage: Int)(using allocator: Allocator)
      extends Buffer(size, usage, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)(using allocator)

  private[cyfra] class HostBuffer(size: Int, usage: Int)(using allocator: Allocator)
      extends Buffer(size, usage, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)(using allocator):
    def mapped(flush: Boolean)(f: ByteBuffer => Unit): Unit = pushStack: stack =>
      val pData = stack.callocPointer(1)
      check(vmaMapMemory(this.allocator.get, this.allocation, pData), "Failed to map buffer to memory")
      val data = pData.get()
      val bb = memByteBuffer(data, size)
      try f(bb)
      finally
        if flush then vmaFlushAllocation(this.allocator.get, this.allocation, 0, size)
        vmaUnmapMemory(this.allocator.get, this.allocation)

    def copyTo(dst: ByteBuffer, srcOffset: Int): Unit = pushStack: stack =>
      vmaCopyAllocationToMemory(allocator.get, allocation, srcOffset, dst)

    def copyFrom(src: ByteBuffer, dstOffset: Int): Unit = pushStack: stack =>
      vmaCopyMemoryToAllocation(allocator.get, src, allocation, dstOffset)

  def copyBuffer(src: Buffer, dst: Buffer, srcOffset: Int, dstOffset: Int, bytes: Int, commandPool: CommandPool)(using Device): Unit = pushStack:
    stack =>
      val cb = copyBufferCommandBuffer(src, dst, srcOffset, dstOffset, bytes, commandPool)

      val pCB = stack.callocPointer(1).put(0, cb)
      val submitInfo = VkSubmitInfo
        .calloc(stack)
        .sType$Default()
        .pCommandBuffers(pCB)

      val fence = Fence()
      check(vkQueueSubmit(commandPool.queue.get, submitInfo, fence.get), "Failed to submit single time command buffer")
      fence.block().destroy()

  def copyBufferCommandBuffer(src: Buffer, dst: Buffer, srcOffset: Int, dstOffset: Int, bytes: Int, commandPool: CommandPool): VkCommandBuffer =
    commandPool.recordSingleTimeCommand: commandBuffer =>
      pushStack: stack =>
        val copyRegion = VkBufferCopy
          .calloc(1, stack)
          .srcOffset(srcOffset)
          .dstOffset(dstOffset)
          .size(bytes)
        vkCmdCopyBuffer(commandBuffer, src.get, dst.get, copyRegion)
