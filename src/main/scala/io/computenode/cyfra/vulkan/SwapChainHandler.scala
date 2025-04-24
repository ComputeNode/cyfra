package io.computenode.cyfra.vulkan

import io.computenode.cyfra.vulkan.core.{Surface, SurfaceManager, SwapChainManager}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.KHRSurface.* 
import org.lwjgl.vulkan.VkSemaphoreCreateInfo
import org.lwjgl.vulkan.VkCommandBuffer 
import org.lwjgl.vulkan.VkQueue 

/**
 * Handles the complete lifecycle of swap chain operations
 * including synchronization primitives and window resize events
 */
class SwapChainHandler(context: VulkanContext) extends AutoCloseable {

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

    true
  }

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