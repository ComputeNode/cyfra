package io.computenode.cyfra.api

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.memory.{Buffer => VkBuffer}
import org.lwjgl.util.vma.Vma.*
import org.lwjgl.vulkan.VK10.*
import java.nio.ByteBuffer
import java.nio.ByteOrder
import org.lwjgl.BufferUtils
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.{Try, Success, Failure}
import org.lwjgl.system.MemoryUtil

/**
 * Represents a buffer in GPU memory
 */
class Buffer(
    private[api] val vulkanContext: VulkanContext, 
    val size: Int, 
    val isHostVisible: Boolean = false
) extends AutoCloseable {
  
  private val closed = new AtomicBoolean(false)
  
  // Create the underlying Vulkan buffer
  private[api] val vkBuffer: VkBuffer = {
    if (isHostVisible) {
      new VkBuffer(
        size,
        VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        VMA_MEMORY_USAGE_CPU_TO_GPU,
        vulkanContext.allocator
      )
    } else {
      new VkBuffer(
        size,
        VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        0,
        VMA_MEMORY_USAGE_CPU_TO_GPU,
        vulkanContext.allocator
      )
    }
  }
  
  /**
   * Copies data from a ByteBuffer to this buffer
   */
  def copyFrom(src: ByteBuffer): Try[Unit] = Try {
    // Save original position
    val origPos = src.position() 
    
    // Make sure we're reading from the start of the buffer
    src.position(0)
    
    // Get a mappable buffer to write to
    val dst = vkBuffer.mapToByteBuffer()
    
    // Reset position of destination
    dst.position(0)
    
    // Copy byte-by-byte (more reliable)
    for (i <- 0 until Math.min(src.remaining(), size)) {
      dst.put(i, src.get(i))
    }
    
    // Restore source position
    src.position(origPos)
    
    // Unmap the buffer
    vkBuffer.unmap()
  }
  
  /**
   * Copy data from this buffer to host memory
   * @return ByteBuffer containing the buffer data
   * @throws IllegalStateException if buffer has been closed
   */
  def copyToHost(): Try[ByteBuffer] = Try {
    // Map the memory
    val srcBuffer = vkBuffer.mapToByteBuffer()
    
    // Explicitly invalidate the memory to ensure host sees device changes
    vmaInvalidateAllocation(vulkanContext.allocator.get, vkBuffer.allocation, 0, size)
    
    // Create a new ByteBuffer to hold the data
    val dstBuffer = BufferUtils.createByteBuffer(size)
    
    // Copy data
    srcBuffer.limit(size)
    srcBuffer.position(0)
    dstBuffer.put(srcBuffer)
    dstBuffer.flip()
    
    // Unmap memory when done
    vkBuffer.unmap()
    
    dstBuffer
  }
  
  /**
   * Creates a new buffer with the same data as this one
   * @return A new Buffer instance
   * @throws IllegalStateException if buffer has been closed
   */
  def duplicate(): Try[Buffer] = Try {
    if (closed.get()) {
      throw new IllegalStateException("Buffer has been closed")
    }
    
    val newBuffer = new Buffer(vulkanContext, size, isHostVisible)
    try {
      VkBuffer.copyBuffer(vkBuffer, newBuffer.vkBuffer, size, vulkanContext.commandPool)
        .block().destroy()
      newBuffer
    } catch {
      case e: Exception =>
        newBuffer.close()
        throw e
    }
  }
  
  /**
   * Map the buffer to host memory (only for host-visible buffers)
   * @return A ByteBuffer mapped to the device memory
   * @throws UnsupportedOperationException if the buffer is not host-visible
   * @throws IllegalStateException if buffer has been closed
   */
  def map(): Try[ByteBuffer] = Try {
    if (closed.get()) {
      throw new IllegalStateException("Buffer has been closed")
    }
    if (!isHostVisible) {
      throw new UnsupportedOperationException("Cannot map a non-host-visible buffer")
    }
    
    var mappedBuffer: ByteBuffer = null
    try {
      mappedBuffer = vkBuffer.mapToByteBuffer()
      
      if (mappedBuffer == null || !mappedBuffer.isDirect() || mappedBuffer.capacity() != size) {
        println("[ERROR] Invalid memory mapping - null, non-direct buffer, or wrong size")
        return Failure(new IllegalStateException("Memory mapping failed"))
      }
      
      // Create a duplicate to avoid direct manipulation of the mapped memory
      val result = BufferUtils.createByteBuffer(size)
      
      // Save original position and limit
      val originalPosition = mappedBuffer.position()
      val originalLimit = mappedBuffer.limit()
      
      // Ensure proper buffer state for copying
      mappedBuffer.position(0).limit(size)
      
      // Copy data
      result.put(mappedBuffer)
      
      // Restore original position and limit
      mappedBuffer.position(originalPosition).limit(originalLimit)
      
      // Prepare result for reading
      result.flip()
      
      result
    } catch {
      case e: Exception =>
        println(s"[ERROR] Buffer mapping failed: ${e.getMessage}")
        throw new RuntimeException("Failed to map buffer", e)
    } finally {
      // Make sure to unmap if needed
      if (mappedBuffer != null) {
        try {
          vkBuffer.unmap()
        } catch {
          case e: Exception => 
            println(s"[ERROR] Failed to unmap buffer: ${e.getMessage}")
        }
      }
    }
  }
    
  /**
   * Get the buffer size in bytes
   */
  def getSize: Int = size
  
  /**
   * Check if buffer is host visible
   */
  def isHostAccessible: Boolean = isHostVisible
  
  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      try {
        // Wait for device operations to complete before destroying
        vulkanContext.deviceWaitIdle() // Use the context method instead of device.waitIdle()
        vkBuffer.destroy()
      } catch {
        case e: Exception => 
          println(s"[ERROR] Failed to properly close buffer: ${e.getMessage}")
          throw new RuntimeException("Buffer close operation failed", e)
      }
    }
  }
}