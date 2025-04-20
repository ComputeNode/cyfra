package io.computenode.cyfra.vulkan.render

import io.computenode.cyfra.vulkan.core.Device
import io.computenode.cyfra.vulkan.memory.{Allocator, Buffer}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack} // Ensure pushStack is imported
import org.lwjgl.system.{MemoryStack, MemoryUtil}
import org.lwjgl.util.vma.Vma.{VMA_MEMORY_USAGE_CPU_TO_GPU, vmaMapMemory, vmaUnmapMemory}
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.KHRSwapchain.* // Add this import
import org.lwjgl.vulkan.{VkBufferImageCopy, VkCommandBufferBeginInfo, VkExtent3D, VkImageMemoryBarrier, VkImageSubresourceLayers, VkOffset3D}

import java.nio.ByteBuffer

/**
 * Records command buffers for transferring rendered images to swap chain images
 */
class RenderCommandBufferRecorder(device: Device, allocator: Allocator) {
  
  /**
   * Records commands to transfer RGBA float data to a swap chain image
   *
   * @param commandBuffer Command buffer to record into
   * @param renderedData Array of RGBA float data from the renderer
   * @param swapChainImage The swap chain image to render to
   * @param width Image width
   * @param height Image height
   */
  def recordTransferToSwapChain(
      commandBuffer: org.lwjgl.vulkan.VkCommandBuffer,
      renderedData: Array[(Float, Float, Float, Float)],
      swapChainImage: Long,
      width: Int,
      height: Int
  ): Unit = {
    pushStack { stack => // Use pushStack helper
      // Begin command buffer
      val beginInfo = VkCommandBufferBeginInfo.calloc(stack)
        .sType$Default()
        .flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
      
      check(
        vkBeginCommandBuffer(commandBuffer, beginInfo),
        "Failed to begin command buffer"
      )
      
      // Create and fill staging buffer with converted data
      val stagingBuffer = createStagingBuffer(renderedData, width, height)
      try {
        // Transition image to transfer destination layout
        transitionImageLayout(
          commandBuffer,
          swapChainImage,
          VK_FORMAT_B8G8R8A8_SRGB, // Common swap chain format
          VK_IMAGE_LAYOUT_UNDEFINED,
          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
          stack
        )
        
        // Copy buffer data to image
        val bufferImageCopy = VkBufferImageCopy.calloc(1, stack) // Explicitly allocate a buffer of size 1
          .bufferOffset(0)
          .bufferRowLength(0) 
          .bufferImageHeight(0) 
          .imageSubresource(
            VkImageSubresourceLayers.calloc(stack)
              .aspectMask(VK_IMAGE_ASPECT_COLOR_BIT)
              .mipLevel(0)
              .baseArrayLayer(0)
              .layerCount(1)
          )
          .imageOffset(VkOffset3D.calloc(stack).x(0).y(0).z(0)) // Pass a VkOffset3D object
          .imageExtent(VkExtent3D.calloc(stack).width(width).height(height).depth(1)) // Pass a VkExtent3D object
        
        vkCmdCopyBufferToImage(
          commandBuffer,
          stagingBuffer.get,
          swapChainImage,
          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
          bufferImageCopy // Pass the buffer
        )
        
        // Transition image to presentation layout
        transitionImageLayout(
          commandBuffer,
          swapChainImage,
          VK_FORMAT_B8G8R8A8_SRGB,
          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
          VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
          stack
        )
      } finally {
        // Clean up staging buffer
        stagingBuffer.destroy()
      }
      
      // End command buffer
      check(
        vkEndCommandBuffer(commandBuffer),
        "Failed to end recording command buffer"
      )
    }
  }
  
  /**
   * Create a staging buffer with rendered data converted to BGRA format
   */
  private def createStagingBuffer(renderedData: Array[(Float, Float, Float, Float)], width: Int, height: Int): Buffer = {
    // 4 bytes per pixel (BGRA8)
    val bufferSize = width * height * 4
    
    // Create staging buffer
    val stagingBuffer = new Buffer(
      bufferSize,
      VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
      VMA_MEMORY_USAGE_CPU_TO_GPU,
      allocator
    )
    
    // Map buffer memory and write converted data
    pushStack { stack => // Use pushStack helper
      val pData = stack.mallocPointer(1)
      check(
        vmaMapMemory(allocator.get, stagingBuffer.allocation, pData),
        "Failed to map staging buffer memory"
      )
      
      val dstPtr = pData.get(0)
      
      // Convert RGBA float to BGRA byte data
      for (i <- renderedData.indices) {
        val (r, g, b, a) = renderedData(i)
        
        // Convert to BGRA with gamma correction
        val byteB = linearToSrgb(b).toByte
        val byteG = linearToSrgb(g).toByte
        val byteR = linearToSrgb(r).toByte
        val byteA = (a * 255).toByte
        
        // Write bytes to mapped memory
        val offset = i * 4
        MemoryUtil.memPutByte(dstPtr + offset, byteB)
        MemoryUtil.memPutByte(dstPtr + offset + 1, byteG)
        MemoryUtil.memPutByte(dstPtr + offset + 2, byteR)
        MemoryUtil.memPutByte(dstPtr + offset + 3, byteA)
      }
      
      // Unmap memory
      vmaUnmapMemory(allocator.get, stagingBuffer.allocation)
    }
    
    stagingBuffer
  }
  
  /**
   * Convert a linear color value to sRGB with gamma correction
   */
  private def linearToSrgb(linear: Float): Int = {
    val v = math.max(0f, math.min(1f, linear))
    val srgb = if (v <= 0.0031308f) {
      v * 12.92f
    } else {
      1.055f * math.pow(v, 1.0f/2.4f).toFloat - 0.055f
    }
    (srgb * 255f).toInt
  }
  
  /**
   * Transition image layout with a pipeline barrier
   */
  private def transitionImageLayout(
      commandBuffer: org.lwjgl.vulkan.VkCommandBuffer,
      image: Long,
      format: Int,
      oldLayout: Int,
      newLayout: Int,
      stack: MemoryStack
  ): Unit = {
    // Allocate a buffer of size 1 and configure the first element
    val barrier = VkImageMemoryBarrier.calloc(1, stack)
      .sType$Default()
      .oldLayout(oldLayout)
      .newLayout(newLayout)
      .srcQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED)
      .dstQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED)
      .image(image)
      .subresourceRange(range => {
        range.aspectMask(VK_IMAGE_ASPECT_COLOR_BIT)
          .baseMipLevel(0)
          .levelCount(1)
          .baseArrayLayer(0)
          .layerCount(1)
      })
    
    var sourceStage = 0
    var destinationStage = 0
    
    if (oldLayout == VK_IMAGE_LAYOUT_UNDEFINED && newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
      barrier.srcAccessMask(0)
      barrier.dstAccessMask(VK_ACCESS_TRANSFER_WRITE_BIT)
      
      sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
      destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT
    } else if (oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL && newLayout == VK_IMAGE_LAYOUT_PRESENT_SRC_KHR) {
      barrier.srcAccessMask(VK_ACCESS_TRANSFER_WRITE_BIT)
      barrier.dstAccessMask(VK_ACCESS_MEMORY_READ_BIT)
      
      sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT
      destinationStage = VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    } else {
      throw new IllegalArgumentException(s"Unsupported layout transition: $oldLayout → $newLayout")
    }
    
    vkCmdPipelineBarrier(
      commandBuffer,
      sourceStage, destinationStage,
      0,
      null, // No memory barriers
      null, // No buffer memory barriers
      barrier // Pass the buffer directly
    )
  }
}