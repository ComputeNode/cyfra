package io.computenode.cyfra.vulkan.render

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.core.{Device, SwapChainManager}
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence, Queue}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.VulkanAssertionError
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkCommandBuffer, VkPresentInfoKHR, VkSubmitInfo, VkSemaphoreCreateInfo, VkQueue} // Added VkQueue

/**
 * Handles synchronization for multi-frame rendering using Vulkan.
 * Creates and manages the semaphores and fences needed to coordinate 
 * image acquisition, rendering, and presentation.
 */
class RenderLoopSynchronizer(context: VulkanContext, maxFramesInFlight: Int = 2) extends AutoCloseable {
  private val device = context.device
  private val queue = context.computeQueue

  // Synchronization primitives
  private val imageAvailableSemaphores = new Array[Long](maxFramesInFlight)
  private val renderFinishedSemaphores = new Array[Long](maxFramesInFlight)
  private val inFlightFences = new Array[Fence](maxFramesInFlight)

  // Track resources used by frames currently in flight
  private var imagesInFlight: Map[Int, Fence] = Map.empty

  // Current frame index (rotates through maxFramesInFlight)
  private var currentFrame = 0

  /**
   * Initialize synchronization resources
   */
  def initialize(): Unit = {
    pushStack { stack => // Use pushStack helper
      val semaphoreInfo = VkSemaphoreCreateInfo.calloc(stack)
        .sType$Default()

      // Create semaphores and fences
      for (i <- 0 until maxFramesInFlight) {
        val pImageAvailable = stack.mallocLong(1)
        val pRenderFinished = stack.mallocLong(1)

        check(
          vkCreateSemaphore(device.get, semaphoreInfo, null, pImageAvailable),
          s"Failed to create image available semaphore $i"
        )
        check(
          vkCreateSemaphore(device.get, semaphoreInfo, null, pRenderFinished),
          s"Failed to create render finished semaphore $i"
        )

        imageAvailableSemaphores(i) = pImageAvailable.get(0)
        renderFinishedSemaphores(i) = pRenderFinished.get(0)

        // Create fence in signaled state initially, so first frame doesn't wait indefinitely
        inFlightFences(i) = new Fence(device, VK_FENCE_CREATE_SIGNALED_BIT)
      }
    }
  }

  /**
   * Begin a new frame - waits for previous work to complete and acquires next image
   *
   * @param swapChain The swap chain to acquire an image from
   * @return The acquired image index or -1 if swap chain is out of date
   */
  def beginFrame(swapChain: SwapChainManager): Int = {
    // Wait for previous frame using this slot to complete
    inFlightFences(currentFrame).block()

    // Acquire next image - no semaphore needed
    val imageIndex = swapChain.acquireNextImage()
    if (imageIndex < 0) {
      return -1 // Swap chain is out of date
    }

    // If this image is already in use by another frame, wait on its fence
    imagesInFlight.get(imageIndex) match {
      case Some(fence) => fence.block()
      case None => // Image not in use, nothing to wait for
    }

    // Mark this image as in use by current frame
    imagesInFlight = imagesInFlight.updated(imageIndex, inFlightFences(currentFrame))

    // Reset fence for this frame
    inFlightFences(currentFrame).reset()

    imageIndex
  }

  /**
   * Submit a command buffer with proper synchronization
   *
   * @param commandBuffer The command buffer to submit
   * @param imageIndex The swap chain image index being rendered to
   */
  def submitCommandBuffer(commandBuffer: VkCommandBuffer, imageIndex: Int): Unit = {
    pushStack { stack => // Use pushStack helper
      // Specify which semaphore to wait on before execution
      val waitSemaphores = stack.mallocLong(1)
      waitSemaphores.put(0, imageAvailableSemaphores(currentFrame))

      // Specify which pipeline stage to wait at
      val waitStages = stack.mallocInt(1)
      waitStages.put(0, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)

      // Specify which semaphore to signal when done
      val signalSemaphores = stack.mallocLong(1)
      signalSemaphores.put(0, renderFinishedSemaphores(currentFrame))

      // Set up command buffer pointers
      val pCommandBuffers = stack.pointers(commandBuffer)

      // Configure submission
      val submitInfo = VkSubmitInfo.calloc(stack)
        .sType$Default()
        .waitSemaphoreCount(1)
        .pWaitSemaphores(waitSemaphores)
        .pWaitDstStageMask(waitStages)
        .pCommandBuffers(pCommandBuffers) // Set the command buffer pointer
        .pSignalSemaphores(signalSemaphores)

      // Submit to queue with fence that signals when rendering completes
      check(
        vkQueueSubmit(queue.get, submitInfo, inFlightFences(currentFrame).get),
        "Failed to submit command buffer to queue"
      )
    }
  }

  /**
   * Present the rendered image to the swap chain
   *
   * @param swapChain The swap chain to present to
   * @param imageIndex The image index to present
   * @param commandBuffer The command buffer containing the rendered frame
   * @param presentQueue The queue to use for presentation
   * @return True if presentation was successful, false if swap chain needs recreation
   */
  def presentFrame(swapChain: SwapChainManager, imageIndex: Int, commandBuffer: VkCommandBuffer, presentQueue: VkQueue): Boolean = {
    // Submit and present - pass command buffer and queue
    val presentResult = swapChain.submitAndPresent(commandBuffer, imageIndex, presentQueue)

    // Advance to next frame
    currentFrame = (currentFrame + 1) % maxFramesInFlight

    presentResult
  }

  /**
   * Clean up synchronization resources
   */
  override def close(): Unit = {
    device.waitIdle()

    // Clean up synchronization objects
    for (i <- 0 until maxFramesInFlight) {
      if (imageAvailableSemaphores(i) != VK_NULL_HANDLE) {
        vkDestroySemaphore(device.get, imageAvailableSemaphores(i), null)
      }

      if (renderFinishedSemaphores(i) != VK_NULL_HANDLE) {
        vkDestroySemaphore(device.get, renderFinishedSemaphores(i), null)
      }

      if (inFlightFences(i) != null) {
        inFlightFences(i).destroy()
      }
    }
  }
}