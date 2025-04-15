package io.computenode.cyfra.vulkan.render

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import io.computenode.cyfra.vulkan.core.{Surface, SurfaceManager, SwapChainManager}
import io.computenode.cyfra.vulkan.memory.Buffer
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.window.{GLFWWindowSystem, WindowEvent, WindowHandle}
import org.lwjgl.glfw.GLFW
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.KHRSwapchain.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
import org.lwjgl.vulkan.VK10._
import org.lwjgl.vulkan.{VkCommandBuffer, VkPresentInfoKHR, VkSemaphoreCreateInfo, VkSubmitInfo}

import java.nio.LongBuffer
import scala.collection.mutable.ArrayBuffer

/**
 * Main render loop that manages the window and Vulkan rendering pipeline
 */
class RenderLoop(
    windowSystem: GLFWWindowSystem,
    context: VulkanContext,
    val initialWidth: Int = 800,
    val initialHeight: Int = 600,
    val windowTitle: String = "Vulkan Render Loop"
) extends AutoCloseable {
  
  // Maximum number of frames that can be processed concurrently
  private val MAX_FRAMES_IN_FLIGHT = 2
  
  // Window and surface resources
  private var window: WindowHandle = _
  private var surfaceManager: SurfaceManager = _
  private var surface: Surface = _
  private var swapChainManager: SwapChainManager = _
  
  // Synchronization primitives
  private val imageAvailableSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT)
  private val renderFinishedSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT)
  private val inFlightFences = new Array[Fence](MAX_FRAMES_IN_FLIGHT)
  private var imagesInFlight: Map[Int, Fence] = Map.empty
  private var currentFrameIndex = 0
  
  // Command buffers - one per swap chain image
  private var commandBuffers: Array[VkCommandBuffer] = _
  
  // Size parameters
  private var width = initialWidth
  private var height = initialHeight
  
  // State tracking
  private var framebufferResized = false
  private var running = false
  
  // Add this public accessor method
  def getSwapChainManager: SwapChainManager = swapChainManager

  // Add a resize listener type and variable
  private var resizeListener: Option[(Int, Int) => Unit] = None

  /**
   * Set a listener for window resize events
   * @param listener Function to call when window is resized
   */
  def setResizeListener(listener: (Int, Int) => Unit): Unit = {
    resizeListener = Some(listener)
  }

  /**
   * Initialize window and Vulkan resources
   */
  def initialize(): Boolean = {
    try {
      // Create window
      window = windowSystem.createWindow(width, height, windowTitle)
      if (window == null) {
        println("Failed to create window")
        return false
      }
      
      // Create surface manager and surface
      surfaceManager = new SurfaceManager(context)
      surface = surfaceManager.createSurface(window.nativePtr)
      
      // Create swap chain
      swapChainManager = new SwapChainManager(context, surface)
      if (!swapChainManager.initialize(width, height)) {
        println("Failed to create swap chain")
        return false
      }
      
      // Create synchronization objects
      createSyncObjects()
      
      // Create command buffers
      createCommandBuffers()
      
      true
    } catch {
      case e: Exception =>
        e.printStackTrace()
        false
    }
  }
  
  /**
   * Create semaphores and fences for frame synchronization
   */
  private def createSyncObjects(): Unit = {
    pushStack { stack =>
      // Initialize semaphore creation info
      val semaphoreInfo = VkSemaphoreCreateInfo.calloc(stack)
        .sType$Default()
      
      // Create semaphores and fences for each frame in flight
      for (i <- 0 until MAX_FRAMES_IN_FLIGHT) {
        val pImageAvailableSemaphore = stack.mallocLong(1)
        val pRenderFinishedSemaphore = stack.mallocLong(1)
        
        check(
          vkCreateSemaphore(context.device.get, semaphoreInfo, null, pImageAvailableSemaphore),
          s"Failed to create image available semaphore $i"
        )
        check(
          vkCreateSemaphore(context.device.get, semaphoreInfo, null, pRenderFinishedSemaphore),
          s"Failed to create render finished semaphore $i"
        )
        
        // Store the semaphore handles
        imageAvailableSemaphores(i) = pImageAvailableSemaphore.get(0)
        renderFinishedSemaphores(i) = pRenderFinishedSemaphore.get(0)
        
        // Create fence in signaled state so first waitForFences succeeds
        inFlightFences(i) = new Fence(context.device, VK_FENCE_CREATE_SIGNALED_BIT)
      }
    }
  }
  
  /**
   * Create command buffers - one for each swap chain image
   */
  private def createCommandBuffers(): Unit = {
    val swapChainImages = swapChainManager.getImages
    commandBuffers = new Array[VkCommandBuffer](swapChainImages.length)
    
    // Allocate command buffers from the command pool
    commandBuffers = context.commandPool.createCommandBuffers(swapChainImages.length).toArray
  }
  
  /**
   * Record command buffer for rendering
   * 
   * @param commandBuffer Command buffer to record into
   * @param imageIndex Index of the swap chain image to render to
   * @param renderFn Optional render function that contains custom rendering commands
   */
  private def recordCommandBuffer(
      commandBuffer: VkCommandBuffer, 
      imageIndex: Int,
      renderFn: Option[(VkCommandBuffer, Int) => Unit] = None
  ): Unit = {
    pushStack { stack =>
      // Begin command buffer recording
      val beginInfo = org.lwjgl.vulkan.VkCommandBufferBeginInfo.calloc(stack)
        .sType$Default()
        .flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
      
      check(
        vkBeginCommandBuffer(commandBuffer, beginInfo),
        "Failed to begin recording command buffer"
      )
      
      // Execute custom rendering function if provided
      renderFn.foreach(_(commandBuffer, imageIndex))
      
      // End command buffer recording
      check(
        vkEndCommandBuffer(commandBuffer),
        "Failed to record command buffer"
      )
    }
  }
  
  /**
   * Run the render loop with a custom render function
   * 
   * @param renderFn Function that records rendering commands for each frame
   */
  def run(renderFn: (VkCommandBuffer, Int) => Unit): Unit = {
    if (!running && window != null) {
      running = true
      
      while (running && !windowSystem.shouldWindowClose(window)) {
        // Poll for window events
        handleEvents()
        
        // Draw a frame
        drawFrame(Some(renderFn))
        
        // Optional: throttle rendering to avoid 100% CPU usage
        Thread.sleep(1)
      }
      
      // Wait for the device to finish all operations before exiting
      context.device.waitIdle()
    }
  }
  
  /**
   * Simple render loop with no custom rendering - useful for testing
   */
  def run(): Unit = {
    run((_, _) => {})
  }
  
  /**
   * Handle window events
   */
  private def handleEvents(): Unit = {
    val events = windowSystem.pollEvents()
    
    for (event <- events) {
      event match {
        case WindowEvent.Resize(newWidth, newHeight) =>
          // Only set the flag if dimensions are non-zero
          if (newWidth > 0 && newHeight > 0) {
            width = newWidth
            height = newHeight
            framebufferResized = true
            
            // Notify listener if registered
            resizeListener.foreach(_(newWidth, newHeight))
          }
          
        case WindowEvent.Close =>
          running = false
          
        case WindowEvent.Key(GLFW.GLFW_KEY_ESCAPE, GLFW.GLFW_PRESS, _) =>
          // Exit on Escape key
          running = false
          
        case _ => // Ignore other events
      }
    }
  }
  
  /**
   * Draw a single frame
   */
  private def drawFrame(renderFn: Option[(VkCommandBuffer, Int) => Unit]): Unit = {
    try {
      // Wait for the previous frame to finish (fence from the current frame index)
      inFlightFences(currentFrameIndex).block()
      
      // Acquire next image - no semaphore needed
      val imageIndex = swapChainManager.acquireNextImage()
      
      // If image acquisition failed (return value -1), recreate the swap chain
      if (imageIndex < 0) {
        if (framebufferResized) {
          recreateSwapChain()
          framebufferResized = false
        }
        return
      }
      
      // Check if this image is still being used by another frame
      imagesInFlight.get(imageIndex).foreach(_.block())
      
      // Mark the image as in use by this frame
      imagesInFlight = imagesInFlight + (imageIndex -> inFlightFences(currentFrameIndex))
      
      // Reset the fence before submitting new work
      inFlightFences(currentFrameIndex).reset()
      
      // Record command buffer with drawing commands
      val commandBuffer = commandBuffers(imageIndex)
      vkResetCommandBuffer(commandBuffer, 0) // Reset the command buffer before recording
      recordCommandBuffer(commandBuffer, imageIndex, renderFn)
      
      // Submit and present - pass command buffer and the compute queue from context
      val presentResult = swapChainManager.submitAndPresent(commandBuffers(imageIndex), imageIndex, context.computeQueue.get) // Use context.computeQueue.get
      
      // Check if we need to recreate the swap chain
      if (!presentResult || framebufferResized) {
        recreateSwapChain()
        framebufferResized = false
        return
      }
      
      // Move to next frame
      currentFrameIndex = (currentFrameIndex + 1) % MAX_FRAMES_IN_FLIGHT
    } catch {
      case e: Exception =>
        e.printStackTrace()
        running = false
    }
  }
  
  /**
   * Submit a command buffer with appropriate synchronization
   */
  private def submitCommandBuffer(
      commandBuffer: VkCommandBuffer,
      waitSemaphore: Long,
      signalSemaphore: Long,
      fence: Fence
  ): Unit = {
    pushStack { stack =>
      // Set up wait semaphore and stage
      val waitSemaphores = stack.mallocLong(1)
      waitSemaphores.put(0, waitSemaphore)
      
      val waitStages = stack.mallocInt(1)
      waitStages.put(0, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
      
      // Set up signal semaphore
      val signalSemaphores = stack.mallocLong(1)
      signalSemaphores.put(0, signalSemaphore)
      
      // Set up command buffer
      val pCommandBuffers = stack.pointers(commandBuffer)
      
      // Submit info
      val submitInfo = VkSubmitInfo.calloc(stack)
        .sType$Default()
        .waitSemaphoreCount(1)
        .pWaitSemaphores(waitSemaphores)
        .pWaitDstStageMask(waitStages)
        .pCommandBuffers(pCommandBuffers)
        .pSignalSemaphores(signalSemaphores)
      
      // Submit to the queue with fence
      check(
        vkQueueSubmit(context.computeQueue.get, submitInfo, fence.get), // Access computeQueue via context and use get
        "Failed to submit draw command buffer"
      )
    }
  }
  
  /**
   * Recreate the swap chain (e.g., after window resize)
   */
  private def recreateSwapChain(): Unit = {
    // If window is minimized (width or height is zero), wait until it's restored
    if (width == 0 || height == 0) {
      while (width == 0 || height == 0) {
        val events = windowSystem.pollEvents()
        for (event <- events) {
          event match {
            case WindowEvent.Resize(w, h) if w > 0 && h > 0 =>
              width = w
              height = h
            case _ =>
          }
        }
        Thread.sleep(100)
      }
    }
    
    // Wait until device is idle before recreating swap chain
    context.device.waitIdle()
    
    // Recreate swap chain with new dimensions
    swapChainManager.initialize(width, height)
    
    // Reset the images in flight since we have a new swap chain
    imagesInFlight = Map.empty
  }
  
  /**
   * Clean up resources
   */
  override def close(): Unit = {
    running = false
    
    if (context != null) {
      context.device.waitIdle()
      
      // Clean up synchronization objects
      for (i <- 0 until MAX_FRAMES_IN_FLIGHT) {
        if (imageAvailableSemaphores(i) != 0) {
          vkDestroySemaphore(context.device.get, imageAvailableSemaphores(i), null)
        }
        if (renderFinishedSemaphores(i) != 0) {
          vkDestroySemaphore(context.device.get, renderFinishedSemaphores(i), null)
        }
        if (inFlightFences(i) != null) {
          inFlightFences(i).close()
        }
      }
      
      // Swap chain automatically cleans up its own image views
      if (swapChainManager != null) {
        swapChainManager.close()
      }
      
      if (surfaceManager != null) {
        // Surface manager will clean up the surface
        surfaceManager.destroy()
      }
    }
    
    // Destroy window
    if (window != null && window.nativePtr != 0) {
      windowSystem.destroyWindow(window)
    }
  }
}