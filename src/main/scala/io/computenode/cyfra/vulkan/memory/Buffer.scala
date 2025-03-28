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
import org.lwjgl.vulkan.{VkBufferCopy, VkBufferCreateInfo, VkCommandBuffer, VkBufferMemoryBarrier}
import org.lwjgl.system.MemoryUtil
import java.nio.ByteOrder

import java.nio.{ByteBuffer, LongBuffer}
import scala.util.Using

/** @author
  *   MarconZet Created 11.05.2019
  */
private[cyfra] class Buffer(val size: Int, val usage: Int, flags: Int, memUsage: Int, val allocator: Allocator) extends VulkanObjectHandle {

  // Add this field to the Buffer class to track mapping state
  private var isMapped = false

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

  /** mapToByteBuffer: Maps GPU memory directly to a ByteBuffer for CPU access */
  def mapToByteBuffer(): ByteBuffer = {
    // Use stack allocation for temporary resources
    pushStack { stack =>
      val pData = stack.callocPointer(1)
      
      // Map the memory
      check(vmaMapMemory(allocator.get, allocation, pData), "Failed to map buffer memory")
      isMapped = true // Mark as mapped
      
      try {
        val mappedAddress = pData.get(0)
        
        if (mappedAddress == 0L) {
          throw new RuntimeException("Failed to map memory: null pointer returned")
        }
        
        // Create a direct ByteBuffer that points to the GPU memory
        val buffer = MemoryUtil.memByteBuffer(mappedAddress, size)
        
        // IMPORTANT: The buffer is now directly mapping GPU memory
        // DO NOT call unmap() until you're done with this buffer!
        
        // Return a duplicate to avoid position/limit changes affecting the original
        val result = buffer.duplicate().order(ByteOrder.nativeOrder())
        result.position(0)
        result.limit(size)
        result
      } catch {
        case e: Exception =>
          // Clean up on error
          vmaUnmapMemory(allocator.get, allocation)
          isMapped = false // Mark as unmapped
          throw e
      }
      // NOTE: We're not unmapping here because the caller needs access to mapped memory
    }
  }

  /** Unmaps memory previously mapped with mapToByteBuffer */
  def unmap(): Unit = {
    try {
      // Check if already unmapped to prevent double unmapping
      if (!isMapped) {
        println("Buffer memory already unmapped or never mapped")
        return
      }
      
      // Perform the unmapping
      vmaUnmapMemory(allocator.get, allocation)
      
      // Mark as unmapped
      isMapped = false
    } catch {
      case e: Exception => 
        println(s"Failed to unmap buffer memory: ${e.getMessage}")
        e.printStackTrace()
        throw e // Rethrow to notify caller
    }
  }

  def getSize: Int = size

  protected def close(): Unit = {
    try {
      if (isMapped) {
        vmaUnmapMemory(allocator.get, allocation)
        isMapped = false
      }
      vmaDestroyBuffer(allocator.get, handle, allocation)
    } catch {
      case e: Exception =>
        println(s"Exception in buffer close: ${e.getMessage}")
        e.printStackTrace()
    }
  }
}

object Buffer {
  // Add this helper method
  def validateSize(requested: Long, available: Int): Int = {
    if (requested <= 0) {
      println("Invalid buffer size requested: " + requested)
      return 0
    }
    if (requested > Int.MaxValue) {
      println(s"Buffer size too large (${requested}), capping at Int.MaxValue")
      return Int.MaxValue
    }
    Math.min(requested.toInt, available)
  }
  
  def copyBuffer(src: ByteBuffer, dst: Buffer, bytes: Int): Unit = {
    // Validate parameters
    if (src == null || dst == null) {
      println("Source or destination buffer is null")
      return
    }
    
    val safeBytes = Math.min(Math.min(src.remaining(), dst.getSize), bytes)
    if (safeBytes <= 0) {
      println(s"Invalid copy size: srcRemaining=${src.remaining()}, " +
              s"dstSize=${dst.getSize}, requested=${bytes}")
      return
    }
    
    // Create a temporary duplicate of the source buffer to avoid position changes
    val srcCopy = src.duplicate()
    srcCopy.limit(Math.min(src.position() + safeBytes, src.limit()))
    
    // Map memory with error checking
    val pData = memAllocPointer(1)
    try {
      val result = vmaMapMemory(dst.allocator.get, dst.allocation, pData)
      
      if (result != 0) {
        println(s"Failed to map memory, error code: $result")
        return
      }
      
      val data = pData.get(0)
      if (data == 0L) {
        println("Mapped memory address is null!")
        return
      }
      
      // Copy with bounds checking
      try {
        memCopy(memAddress(srcCopy), data, safeBytes)
      } catch {
        case e: Exception =>
          println(s"Exception during memory copy: ${e.getMessage}")
          e.printStackTrace()
      }
    } catch {
      case e: Exception =>
        println(s"Exception during buffer copy: ${e.getMessage}")
        e.printStackTrace()
    } finally {
      try {
        vmaUnmapMemory(dst.allocator.get, dst.allocation)
      } catch {
        case e: Exception => 
          println(s"Failed to unmap memory: ${e.getMessage}")
      }
      memFree(pData)
    }
  }

  def copyBuffer(src: ByteBuffer, dst: Buffer, bytes: Long): Unit = {
    val safeBytes = Math.min(Math.min(src.remaining(), dst.getSize), bytes)
    if (safeBytes <= 0) {
      // Fix: Don't return Failure in a method returning Unit
      println("Invalid copy size")
      return // Just return without copying anything
    }
    
    val srcCopy = src.duplicate()
    srcCopy.order(ByteOrder.nativeOrder())  // Use native order

    pushStack { stack =>
      val pData = stack.callocPointer(1)
      check(vmaMapMemory(dst.allocator.get, dst.allocation, pData), "Failed to map destination buffer memory")
      val data = pData.get()
      memCopy(memAddress(srcCopy), data, safeBytes)
      vmaFlushAllocation(dst.allocator.get, dst.allocation, 0, safeBytes)
      vmaUnmapMemory(dst.allocator.get, dst.allocation)
    }
  }

  def copyBuffer(src: Buffer, dst: ByteBuffer, bytes: Long): Unit =
    pushStack { stack =>
      val safeBytes = validateSize(bytes, Math.min(src.getSize, dst.remaining()))
      if (safeBytes <= 0) {
        println("Cannot copy zero or negative bytes")
        return
      }
      
      val pData = stack.callocPointer(1)
      check(vmaMapMemory(src.allocator.get, src.allocation, pData), "Failed to map destination buffer memory")
      val data = pData.get()
      
      // Copy the data
      memCopy(data, memAddress(dst), safeBytes)
      
      // Update the position of the destination buffer
      dst.position(dst.position() + safeBytes)
      
      vmaUnmapMemory(src.allocator.get, src.allocation)
    }

  def copyBuffer(src: Buffer, dst: Buffer, bytes: Long, commandPool: CommandPool): Fence = {
    // Validate parameters
    if (src == null || dst == null) {
      throw new IllegalArgumentException("Source or destination buffer is null")
    }
    
    val safeBytes = validateSize(bytes, Math.min(src.getSize, dst.getSize))
    if (safeBytes <= 0) {
      throw new IllegalArgumentException("Invalid copy size: " + bytes)
    }
    
    pushStack { stack =>
      val commandBuffer = commandPool.beginSingleTimeCommands()
      
      // Source buffer memory barrier - ensure writes are visible
      val srcBarrier = VkBufferMemoryBarrier.calloc(1, stack)
        .sType(VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
        .srcAccessMask(VK_ACCESS_MEMORY_WRITE_BIT)
        .dstAccessMask(VK_ACCESS_TRANSFER_READ_BIT)
        .srcQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED)
        .dstQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED)
        .buffer(src.get)
        .offset(0)
        .size(safeBytes)

      vkCmdPipelineBarrier(
        commandBuffer,
        VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
        VK_PIPELINE_STAGE_TRANSFER_BIT,
        0,
        null,
        srcBarrier,
        null
      )
      
      // Execute the copy
      val copyRegion = VkBufferCopy.calloc(1, stack)
        .srcOffset(0)
        .dstOffset(0)
        .size(safeBytes)
      vkCmdCopyBuffer(commandBuffer, src.get, dst.get, copyRegion)
      
      // Destination buffer barrier - ensure reads happen after copy
      val dstBarrier = VkBufferMemoryBarrier.calloc(1, stack)
        .sType(VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
        .srcAccessMask(VK_ACCESS_TRANSFER_WRITE_BIT)
        .dstAccessMask(VK_ACCESS_MEMORY_READ_BIT)
        .srcQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED)
        .dstQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED)
        .buffer(dst.get)
        .offset(0)
        .size(safeBytes)

      vkCmdPipelineBarrier(
        commandBuffer,
        VK_PIPELINE_STAGE_TRANSFER_BIT,
        VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
        0,
        null,
        dstBarrier,
        null
      )
      
      commandPool.endSingleTimeCommands(commandBuffer)
    }
  }
  
  /** wrapByteBuffer: Takes an existing ByteBuffer and wraps it with a new GPU buffer object, so you can manage that data in Vulkan. */
  def wrapByteBuffer(data: ByteBuffer, allocator: Allocator): Buffer = {
    val buf = new Buffer(data.remaining(), VK_BUFFER_USAGE_STORAGE_BUFFER_BIT, 0, VMA_MEMORY_USAGE_CPU_TO_GPU, allocator)
    val bytes = new Array[Byte](data.remaining())
    data.get(bytes)
    data.rewind()
    Buffer.copyBuffer(ByteBuffer.wrap(bytes), buf, bytes.length)
    buf
  }
}
