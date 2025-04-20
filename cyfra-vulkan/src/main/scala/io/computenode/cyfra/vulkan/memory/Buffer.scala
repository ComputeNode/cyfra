package io.computenode.cyfra.vulkan.memory

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.PointerBuffer
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.util.vma.Vma.*
import org.lwjgl.util.vma.VmaAllocationCreateInfo
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkBufferCopy, VkBufferCreateInfo, VkCommandBuffer}

import java.nio.{ByteBuffer, LongBuffer}
import scala.util.Using

/** @author
  *   MarconZet Created 11.05.2019
  */
private[cyfra] class Buffer(val size: Int, val usage: Int, flags: Int, memUsage: Int, val allocator: Allocator) extends VulkanObjectHandle {

  val (handle, allocation) = pushStack { stack =>
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
      .usage(memUsage)
      .requiredFlags(flags)

    val pBuffer = stack.callocLong(1)
    val pAllocation = stack.callocPointer(1)
    check(vmaCreateBuffer(allocator.get, bufferInfo, allocInfo, pBuffer, pAllocation, null), "Failed to create buffer")
    (pBuffer.get(), pAllocation.get())
  }

  def get(dst: Array[Byte]): Unit = {
    val len = Math.min(dst.length, size)
    val byteBuffer = memCalloc(len)
    Buffer.copyBuffer(this, byteBuffer, len)
    byteBuffer.get(dst)
    memFree(byteBuffer)
  }

  protected def close(): Unit =
    vmaDestroyBuffer(allocator.get, handle, allocation)
}

object Buffer {
  def copyBuffer(src: ByteBuffer, dst: Buffer, bytes: Long): Unit =
    pushStack { stack =>
      val pData = stack.callocPointer(1)
      check(vmaMapMemory(dst.allocator.get, dst.allocation, pData), "Failed to map destination buffer memory")
      val data = pData.get()
      memCopy(memAddress(src), data, bytes)
      vmaFlushAllocation(dst.allocator.get, dst.allocation, 0, bytes)
      vmaUnmapMemory(dst.allocator.get, dst.allocation)
    }

  def copyBuffer(src: Buffer, dst: ByteBuffer, bytes: Long): Unit =
    pushStack { stack =>
      val pData = stack.callocPointer(1)
      check(vmaMapMemory(src.allocator.get, src.allocation, pData), "Failed to map destination buffer memory")
      val data = pData.get()
      memCopy(data, memAddress(dst), bytes)
      vmaUnmapMemory(src.allocator.get, src.allocation)
    }

  def copyBuffer(src: Buffer, dst: Buffer, bytes: Long, commandPool: CommandPool): Fence =
    pushStack { stack =>
      val commandBuffer = commandPool.beginSingleTimeCommands()

      val copyRegion = VkBufferCopy
        .calloc(1, stack)
        .srcOffset(0)
        .dstOffset(0)
        .size(bytes)
      vkCmdCopyBuffer(commandBuffer, src.get, dst.get, copyRegion)

      commandPool.endSingleTimeCommands(commandBuffer)
    }

}
