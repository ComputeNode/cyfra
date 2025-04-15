package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.core.{Surface, SurfaceManager, SwapChainManager}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack} // Import pushStack
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.KHRSurface.* // Add this import
import org.lwjgl.vulkan.VkSemaphoreCreateInfo
import org.lwjgl.vulkan.VkCommandBuffer // Import VkCommandBuffer
import org.lwjgl.vulkan.VkQueue // Import VkQueue

/**
 * Handles the complete lifecycle of swap chain operations
 * including synchronization primitives and window resize events
 */
class SwapChainHandler(context: VulkanContext) extends AutoCloseable {
  // Synchronization resources managed by SwapChainManager
  // private val MAX_FRAMES_IN_FLIGHT = 2 // No longer needed here
  // private val imageAvailableSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT) // No longer needed here
  // private val renderFinishedSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT) // No longer needed here
  // private var currentFrameIndex = 0 // No longer needed here, managed by SwapChainManager

  // Surface and swap chain managers
  private var surfaceManager: SurfaceManager = new SurfaceManager(context)
  private var swapChainManager: SwapChainManager = _
  private var surface: Surface = _

  // Window dimensions
  private var currentWidth = 0
  private var currentHeight = 0
  private var framesSinceResize = 0
  private var resizeRequested = false

  /**
   * Initialize with a window handle
   *
   * @param windowHandle GLFW window handle
   * @param width Initial width
   * @param height Initial height
   * @param preferredFormat Preferred image format (defaults to VK_FORMAT_B8G8R8A8_SRGB)
   * @param preferredColorSpace Preferred color space (defaults to VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
   */
  def initialize(
    windowHandle: Long,
    width: Int,
    height: Int,
    preferredFormat: Int = VK_FORMAT_B8G8R8A8_SRGB,
    preferredColorSpace: Int = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR // Now resolved
  ): Boolean = {
    currentWidth = width
    currentHeight = height

    // Create surface
    surface = surfaceManager.createSurface(windowHandle)

    // Create swap chain manager
    swapChainManager = new SwapChainManager(context, surface)

    // Initialize swap chain
    if (!swapChainManager.initialize(width, height)) {
      println("Failed to initialize swap chain")
      return false
    }

    // Synchronization primitives are now created inside SwapChainManager
    // createSyncObjects() // No longer needed here

    true
  }

  /**
   * Create semaphores for synchronization - REMOVED as SwapChainManager handles this
   */
  // private def createSyncObjects(): Unit = { ... } // Removed

  /**
   * Notify the handler that a window resize has occurred
   */
  def notifyResize(width: Int, height: Int): Unit = {
    // Only update if dimensions actually changed
    if (width != currentWidth || height != currentHeight) {
      currentWidth = width
      currentHeight = height
      resizeRequested = true
    }
  }

  /**
   * Acquire the next image for rendering
   *
   * @return Image index or -1 if swap chain needs recreation
   */
  def acquireNextImage(): Int = {
    // Handle pending resize immediately
    if (resizeRequested) {
      recreateSwapChain()
      resizeRequested = false
      framesSinceResize = 0
      // Need to re-acquire after resize
      return -1 // Indicate failure/need retry
    }

    // SwapChainManager now handles synchronization internally
    // val imageAvailableSemaphore = imageAvailableSemaphores(currentFrameIndex) // No longer needed

    // Try to acquire an image - no semaphore argument needed
    val imageIndex = swapChainManager.acquireNextImage()

    // If acquisition failed (e.g., out of date), recreate the swap chain
    if (imageIndex < 0) {
      recreateSwapChain()
      return -1 // Indicate failure/need retry
    }

    imageIndex
  }

  /**
   * Present the rendered image by submitting the command buffer
   *
   * @param commandBuffer The command buffer containing rendering commands for the image
   * @param imageIndex Index of the image to present
   * @param queue The queue to submit to and present on
   * @return True if presentation was successful
   */
  def presentImage(commandBuffer: VkCommandBuffer, imageIndex: Int, queue: VkQueue): Boolean = {
    // SwapChainManager handles synchronization internally
    // val renderFinishedSemaphore = renderFinishedSemaphores(currentFrameIndex) // No longer needed

    // Use submitAndPresent which handles submission and presentation
    val result = swapChainManager.submitAndPresent(commandBuffer, imageIndex, queue)

    // Frame index is managed internally by SwapChainManager
    // currentFrameIndex = (currentFrameIndex + 1) % MAX_FRAMES_IN_FLIGHT // No longer needed

    // Track frames since last resize
    framesSinceResize += 1

    // If presentation failed or it's been many frames since resize, request recreation
    // Note: submitAndPresent returns false if swapchain is suboptimal/out-of-date
    if (!result) {
      resizeRequested = true // Request recreation on next acquire
      return false
    } else if (framesSinceResize > 300 && resizeRequested) {
      // Optional: Force recreate if resize was requested long ago but presentation succeeded
      recreateSwapChain()
      return false // Indicate recreation happened
    }

    true
  }

  /**
   * Get the current image available semaphore (from SwapChainManager)
   */
  def getCurrentImageAvailableSemaphore: Long = {
    swapChainManager.getImageAvailableSemaphore // Delegate to SwapChainManager
  }

  /**
   * Get the current render finished semaphore (from SwapChainManager)
   */
  def getCurrentRenderFinishedSemaphore: Long = {
    swapChainManager.getRenderFinishedSemaphore // Delegate to SwapChainManager
  }
  
  /**
   * Get the current in-flight fence (from SwapChainManager)
   */
  def getCurrentInFlightFence: Long = {
    swapChainManager.getInFlightFence // Delegate to SwapChainManager
  }


  /**
   * Recreate the swap chain (e.g., after window resize)
   */
  def recreateSwapChain(): Boolean = {
    // Skip if dimensions are zero (minimized window)
    if (currentWidth == 0 || currentHeight == 0) {
      println("Skipping swapchain recreation: window minimized.")
      return false
    }

    println(s"Recreating swap chain with dimensions: ${currentWidth}x${currentHeight}")
    // Wait for device to be idle before recreating
    context.device.waitIdle()

    // Recreate with new dimensions
    val success = swapChainManager.initialize(currentWidth, currentHeight)

    if (success) {
      println("Swap chain recreated successfully.")
      framesSinceResize = 0
      resizeRequested = false
    } else {
      println("Failed to recreate swap chain.")
    }

    success
  }

  /**
   * Clean up all resources
   */
  override def close(): Unit = {
    println("Closing SwapChainHandler...")
    // Wait for device to be idle before cleanup
    context.device.waitIdle()

    // Clean up semaphores - REMOVED as SwapChainManager handles this
    // for (i <- 0 until MAX_FRAMES_IN_FLIGHT) { ... } // Removed

    // Close swap chain manager
    if (swapChainManager != null) {
      println("Closing SwapChainManager...")
      swapChainManager.close()
      swapChainManager = null // Help GC
    }

    // Close surface manager (automatically cleans up surfaces)
    if (surfaceManager != null) {
      println("Destroying SurfaceManager...")
      surfaceManager.destroy()
      surfaceManager = null // Help GC
    }
    println("SwapChainHandler closed.")
  }

  /**
   * Get the swap chain manager
   */
  def getSwapChainManager: SwapChainManager = swapChainManager
}