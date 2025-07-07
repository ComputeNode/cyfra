package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle
import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.util.vma.Vma.*
import org.lwjgl.util.vma.VmaAllocationCreateInfo
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkBufferCopy, VkBufferCreateInfo}

import java.nio.ByteBuffer

/** @author
  *   MarconZet Created 11.05.2019
  */

private[cyfra] sealed class Buffer private (val size: Int, usage: Int, flags: Int)(using allocator: Allocator) extends VulkanObjectHandle:
  val (handle, allocation) = pushStack: stack =>
    val bufferInfo = VkBufferCreateInfo
      .calloc(stack)
      .sType$Default()
      .pNext(NULL)
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
  private[cyfra] class DeviceLocal(size: Int, usage: Int)(using allocator: Allocator)
      extends Buffer(size, usage, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)(using allocator)

  private[cyfra] class Host(size: Int, usage: Int)(using allocator: Allocator)
      extends Buffer(size, usage, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)(using allocator):
    def mapped(f: ByteBuffer => Unit): Unit = mappedImpl(f, flush = true)
    def mappedNoFlush(f: ByteBuffer => Unit): Unit = mappedImpl(f, flush = false)

    private def mappedImpl(f: ByteBuffer => Unit, flush: Boolean): Unit = pushStack: stack =>
      val pData = stack.callocPointer(1)
      check(vmaMapMemory(this.allocator.get, this.allocation, pData), "Failed to map buffer to memory")
      val data = pData.get()
      val bb = memByteBuffer(data, size)
      try f(bb)
      finally
        if flush then vmaFlushAllocation(this.allocator.get, this.allocation, 0, size)
        vmaUnmapMemory(this.allocator.get, this.allocation)

  def copyBuffer(src: ByteBuffer, dst: Host, bytes: Long): Unit =
    dst.mapped: destination =>
      memCopy(memAddress(src), memAddress(destination), bytes)

  def copyBuffer(src: Host, dst: ByteBuffer, bytes: Long): Unit =
    src.mappedNoFlush: source =>
      memCopy(memAddress(source), memAddress(dst), bytes)

  def copyBuffer(src: Buffer, dst: Buffer, bytes: Long, commandPool: CommandPool): Fence =
    commandPool.executeCommand: commandBuffer =>
      pushStack: stack =>
        val copyRegion = VkBufferCopy
          .calloc(1, stack)
          .srcOffset(0)
          .dstOffset(0)
          .size(bytes)
        vkCmdCopyBuffer(commandBuffer, src.get, dst.get, copyRegion)
