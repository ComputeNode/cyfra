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

  def map[R](f: ByteBuffer => R): R = {
    var dataPtr: Long = NULL
    try {
      dataPtr = pushStack { stack =>
        val pData = stack.callocPointer(1)
        check(vmaMapMemory(allocator.get, allocation, pData), s"Failed to map buffer memory for buffer handle $handle allocation $allocation")
        val ptr = pData.get(0)
        if (ptr == NULL) {
          throw new VulkanAssertionError(s"vmaMapMemory returned NULL for buffer handle $handle, allocation $allocation", -1)
        }
        ptr
      }
      val byteBuffer = memByteBuffer(dataPtr, this.size)
      f(byteBuffer)
    } finally {
      if (dataPtr != NULL) {
        vmaUnmapMemory(allocator.get, allocation)
      }
    }
  }

  def get(dst: Array[Byte]): Unit = {
    val len = Math.min(dst.length, size)
    this.map { mappedBuffer =>
      val bufferSlice = mappedBuffer.slice() 
      bufferSlice.limit(len)
      bufferSlice.get(dst, 0, len) 
    }
  }

  protected def close(): Unit =
    vmaDestroyBuffer(allocator.get, handle, allocation)
}

object Buffer {
  def copyBuffer(src: ByteBuffer, dst: Buffer, bytes: Long): Unit = {
    dst.map { dstMappedBuffer =>
      val srcSlice = src.slice()
      srcSlice.limit(bytes.toInt) 
      dstMappedBuffer.put(srcSlice)
      vmaFlushAllocation(dst.allocator.get, dst.allocation, 0, bytes)
    }
  }

  def copyBuffer(src: Buffer, dst: ByteBuffer, bytes: Long): Unit =
    src.map { srcMappedBuffer =>
      val srcSlice = srcMappedBuffer.slice()
      srcSlice.limit(bytes.toInt)
      dst.put(srcSlice)
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
