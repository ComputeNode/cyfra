package io.computenode.cyfra.vulkan.renderer

import io.computenode.cyfra.dsl.RGBA
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import io.computenode.cyfra.vulkan.core.{Surface, SwapChainManager}
import io.computenode.cyfra.vulkan.render.RenderCommandBufferRecorder // Add this import
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack} // Import pushStack
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkCommandBuffer, VkPresentInfoKHR, VkSemaphoreCreateInfo, VkSubmitInfo}

/**
 * Handles rendering to a swap chain, including synchronization and command buffer submission
 */
class SwapChainRenderer(context: VulkanContext) extends AutoCloseable {
  private val device = context.device
  private val commandPool = context.commandPool
  private val allocator = context.allocator
  
  // Command buffer recorder for transfer operations
  private val commandRecorder = new RenderCommandBufferRecorder(device, allocator)
  
  // Maximum number of frames being processed concurrently
  private val MAX_FRAMES_IN_FLIGHT = 2
  
  // Synchronization primitives
  private val imageAvailableSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT)
  private val renderFinishedSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT)
  private val inFlightFences = new Array[Fence](MAX_FRAMES_IN_FLIGHT)
  private var currentFrame = 0
  
  // Command buffers - one per swap chain image
  private var commandBuffers: Array[VkCommandBuffer] = _
  
  /**
   * Initialize the renderer
   *
   * @param swapChainManager The swap chain manager to use
   * @return Success/failure
   */
  def initialize(swapChainManager: SwapChainManager): Boolean = {
    try {
      createSyncObjects()
      createCommandBuffers(swapChainManager.getImages.length)
      true
    } catch {
      case e: Exception =>
        println(s"Failed to initialize swap chain renderer: ${e.getMessage}")
        e.printStackTrace()
        false
    }
  }
  
  /**
   * Create semaphores and fences for synchronization
   */
  private def createSyncObjects(): Unit = {
    pushStack { stack => // Use pushStack
      val semaphoreInfo = VkSemaphoreCreateInfo.calloc(stack)
        .sType$Default()
      
      // Create semaphores and fences for each concurrent frame
      for (i <- 0 until MAX_FRAMES_IN_FLIGHT) {
        val pImageAvailableSemaphore = stack.mallocLong(1)
        val pRenderFinishedSemaphore = stack.mallocLong(1)
        
        check(
          vkCreateSemaphore(device.get, semaphoreInfo, null, pImageAvailableSemaphore),
          "Failed to create image available semaphore"
        )
        check(
          vkCreateSemaphore(device.get, semaphoreInfo, null, pRenderFinishedSemaphore),
          "Failed to create render finished semaphore"
        )
        
        imageAvailableSemaphores(i) = pImageAvailableSemaphore.get(0)
        renderFinishedSemaphores(i) = pRenderFinishedSemaphore.get(0)
        
        // Create fence in signaled state initially so first wait works
        inFlightFences(i) = new Fence(device, VK_FENCE_CREATE_SIGNALED_BIT)
      }
    }
  }
  
  /**
   * Create command buffers for recording commands
   */
  private def createCommandBuffers(count: Int): Unit = {
    commandBuffers = commandPool.createCommandBuffers(count).toArray
  }
  
  /**
   * Render a frame to the swap chain
   *
   * @param swapChainManager The swap chain manager
   * @param renderedData The RGBA float data to render
   * @param width Image width
   * @param height Image height
   * @return true if rendering was successful, false if swap chain needs recreation
   */
  def renderFrame(
      swapChainManager: SwapChainManager,
      renderedData: Array[RGBA],
      width: Int,
      height: Int
  ): Boolean = {
    // Get synchronization objects for current frame
    val imageAvailableSemaphore = imageAvailableSemaphores(currentFrame)
    val renderFinishedSemaphore = renderFinishedSemaphores(currentFrame)
    val inFlightFence = inFlightFences(currentFrame)
    
    // Wait for previous frame to complete
    inFlightFence.block()
    inFlightFence.reset()
    
    // Acquire next image - no semaphore needed
    val imageIndex = swapChainManager.acquireNextImage()
    if (imageIndex < 0) {
      // Swap chain is out of date, needs recreation
      return false
    }
    
    // Get command buffer for this image
    val commandBuffer = commandBuffers(imageIndex)
    
    // Reset and record command buffer
    vkResetCommandBuffer(commandBuffer, 0)
    
    // Record commands to transfer rendered image to swap chain
    commandRecorder.recordTransferToSwapChain(
      commandBuffer,
      renderedData,
      swapChainManager.getImages(imageIndex), // Corrected line
      width,
      height
    )
    
    // Submit command buffer
    pushStack { stack => // Use pushStack
      // Set up wait stage and semaphore
      val waitSemaphores = stack.mallocLong(1)
      waitSemaphores.put(0, imageAvailableSemaphore)
      
      val waitStages = stack.mallocInt(1)
      waitStages.put(0, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
      
      // Set up signal semaphore
      val signalSemaphores = stack.mallocLong(1)
      signalSemaphores.put(0, renderFinishedSemaphore)
      
      // Set up command buffer
      val pCommandBuffers = stack.pointers(commandBuffer)
      
      // Configure submission
      val submitInfo = VkSubmitInfo.calloc(stack)
        .sType$Default()
        .pWaitSemaphores(waitSemaphores)      // Set the pointer to the wait semaphores
        .pWaitDstStageMask(waitStages)        // Set the wait stages
        .pCommandBuffers(pCommandBuffers)    // Set the command buffers
        .pSignalSemaphores(signalSemaphores) // Set the pointer to the signal semaphores
      
      // Submit to queue
      check(
        vkQueueSubmit(context.computeQueue.get, submitInfo, inFlightFence.get), // Access computeQueue via context
        "Failed to submit render command buffer"
      )
    }
    
    // Submit and present - pass command buffer and the compute queue from context
    val presentSuccess = swapChainManager.submitAndPresent(commandBuffers(imageIndex), imageIndex, context.computeQueue.get) // Use context.computeQueue.get
    
    // Update frame index
    currentFrame = (currentFrame + 1) % MAX_FRAMES_IN_FLIGHT
    
    presentSuccess
  }
  
  /**
   * Clean up resources
   */
  override def close(): Unit = {
    // Wait for device to be idle before cleanup
    device.waitIdle()
    
    // Clean up synchronization primitives
    for (i <- 0 until MAX_FRAMES_IN_FLIGHT) {
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
    
    // Command buffers don't need explicit destruction as they're owned by the command pool
  }
}