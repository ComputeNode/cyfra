package io.computenode.cyfra.vulkan.render

import io.computenode.cyfra.dsl.RGBA
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.command.{CommandPool, Fence}
import io.computenode.cyfra.vulkan.core.{Surface, SurfaceManager, SwapChainManager}
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.window.{GLFWWindowSystem, WindowEvent, WindowHandle}
import org.lwjgl.glfw.GLFW
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.KHRSwapchain.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
import org.lwjgl.vulkan.VK10._
import org.lwjgl.vulkan.{VkCommandBuffer, VkPresentInfoKHR, VkSemaphoreCreateInfo, VkSubmitInfo}
import java.lang.Thread
import java.nio.LongBuffer
import scala.collection.mutable.ArrayBuffer

/**
 * Main Vulkan render loop: manages synchronization, swap chain, and frame pacing.
 * Integrates event handling and resource management for robust rendering.
 */
class VulkanRenderLoop(windowSystem: GLFWWindowSystem, context: VulkanContext) extends AutoCloseable {
  // Number of frames processed concurrently (double buffering)
  private val MAX_FRAMES_IN_FLIGHT = 2

  // Vulkan handles and managers
  private var window: WindowHandle = _
  private var surfaceManager: SurfaceManager = _
  private var surface: Surface = _
  private var swapChainManager: SwapChainManager = _
  private val commandPool = context.commandPool

  // Per-frame synchronization
  private val imageAvailableSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT)
  private val renderFinishedSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT)
  private val inFlightFences = new Array[Fence](MAX_FRAMES_IN_FLIGHT)
  private var imagesInFlight: Map[Int, Fence] = Map.empty
  private var currentFrameIndex = 0

  // Frame timing and stats
  private var targetFPS = 60
  private var limitFrameRate = true
  private var lastFrameTime = 0.0
  private var frameTimes = new ArrayBuffer[Double](100)
  private var frameCounter = 0
  private var fpsUpdateTime = 0.0

  // Window state
  private var width = 800
  private var height = 600
  private var windowTitle = "Vulkan Render"
  private var isRunning = false
  private var framebufferResized = false

  // Command buffers, one per swap chain image
  private var commandBuffers: Array[VkCommandBuffer] = _

  // Optional window resize callback
  private var resizeListener: Option[(Int, Int) => Unit] = None

  /**
   * Register a callback for window resize events.
   */
  def setResizeListener(listener: (Int, Int) => Unit): Unit = {
    resizeListener = Some(listener)
  }

  /**
   * Access the current swap chain manager.
   */
  def getSwapChainManager: SwapChainManager = swapChainManager

  /**
   * Initialize window, Vulkan surface, swap chain, command buffers, and sync objects.
   * @return true if successful, false otherwise.
   */
  def initialize(title: String, initialWidth: Int, initialHeight: Int): Boolean = {
    windowTitle = title
    width = initialWidth
    height = initialHeight
    
    // Create window
    window = windowSystem.createWindow(width, height, windowTitle)
    if (window == null || window.nativePtr == 0) {
      println("Failed to create window")
      return false
    }
    
    // Create surface
    surfaceManager = new SurfaceManager(context)
    surface = surfaceManager.createSurface(window.nativePtr)
    
    // Create swap chain
    swapChainManager = new SwapChainManager(context, surface)
    if (!swapChainManager.initialize(width, height)) {
      println("Failed to initialize swap chain")
      return false
    }
    
    // Create command buffers - one for each swap chain image
    createCommandBuffers()
    
    // Create synchronization objects
    createSyncObjects()
    
    true
  }

  /**
   * Allocate command buffers for each swap chain image.
   */
  private def createCommandBuffers(): Unit = {
    val swapChainImages = swapChainManager.getImages
    commandBuffers = new Array[VkCommandBuffer](swapChainImages.length)
    
    // Allocate command buffers from the command pool
    commandBuffers = context.commandPool.createCommandBuffers(swapChainImages.length).toArray
  }

  /**
   * Create semaphores and fences for frame synchronization.
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
   * Set target frames per second for pacing.
   */
  def setTargetFPS(fps: Int): Unit = {
    targetFPS = fps
  }

  /**
   * Enable or disable frame rate limiting.
   */
  def setLimitFrameRate(limit: Boolean): Unit = {
    limitFrameRate = limit
  }

  /**
   * Get current measured FPS.
   */
  def getCurrentFPS: Double = {
    if (frameTimes.isEmpty) 0.0
    else frameTimes.length / frameTimes.sum
  }

  /**
   * Record rendering commands into a command buffer.
   */
  private def recordCommandBuffer(
      commandBuffer: VkCommandBuffer, 
      imageIndex: Int,
      renderFn: (VkCommandBuffer, Int) => Unit
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
      
      // Execute rendering function
      renderFn(commandBuffer, imageIndex)
      
      // End command buffer recording
      check(
        vkEndCommandBuffer(commandBuffer),
        "Failed to record command buffer"
      )
    }
  }

  /**
   * Main rendering loop: handles events, draws frames, and applies pacing.
   */
  def run(renderFunction: (VkCommandBuffer, Int) => Unit): Unit = {
    if (window == null || !isInitialized) {
      println("Cannot run: window or Vulkan resources not initialized")
      return
    }
    
    isRunning = true
    lastFrameTime = GLFW.glfwGetTime()
    fpsUpdateTime = lastFrameTime
    
    while (isRunning && !windowSystem.shouldWindowClose(window)) {
      val frameStart = GLFW.glfwGetTime()
      
      // Handle window events
      handleEvents()
      
      // Render a frame
      drawFrame(renderFunction)
      
      // Measure frame time
      updateFrameStats(frameStart)
      
      // Frame pacing - sleep if needed
      if (limitFrameRate) {
        applyFramePacing(frameStart)
      }
    }
    
    // Wait for device to be idle before exit
    context.device.waitIdle()
  }

  /**
   * Poll and process window events.
   */
  private def handleEvents(): Unit = {
    val events = windowSystem.pollEvents()
    
    for (event <- events) {
      event match {
        case WindowEvent.Resize(newWidth, newHeight) =>
          if (newWidth > 0 && newHeight > 0 && (width != newWidth || height != newHeight)) {
            width = newWidth
            height = newHeight
            framebufferResized = true
            
            // Notify resize listener if registered
            resizeListener.foreach(_(newWidth, newHeight))
          }
        
        case WindowEvent.Close =>
          isRunning = false
        
        case WindowEvent.Key(GLFW.GLFW_KEY_ESCAPE, GLFW.GLFW_PRESS, _) =>
          // Exit on Escape key
          isRunning = false
        
        case _ => // Ignore other events
      }
    }
  }

  /**
   * Render a frame with synchronization and presentation.
   */
  private def drawFrame(renderFunction: (VkCommandBuffer, Int) => Unit): Unit = {
    try {
      // Wait for the previous frame to finish
      inFlightFences(currentFrameIndex).block()
      
      // Acquire next image - no semaphore parameter needed
      val imageIndex = swapChainManager.acquireNextImage()
      
      // If image acquisition failed, recreate the swap chain
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
      recordCommandBuffer(commandBuffer, imageIndex, renderFunction)
      
      // Submit command buffer with proper synchronization
      submitCommandBuffer(
        commandBuffer,
        imageAvailableSemaphores(currentFrameIndex),
        renderFinishedSemaphores(currentFrameIndex),
        inFlightFences(currentFrameIndex)
      )
      
      // Present the rendered image using submitAndPresent instead of presentImage
      val presentSuccess = swapChainManager.submitAndPresent(
        commandBuffer,
        imageIndex, 
        context.computeQueue.get
      )
      
      // Check if we need to recreate the swap chain
      if (!presentSuccess || framebufferResized) {
        recreateSwapChain()
        framebufferResized = false
        return
      }
      
      // Move to next frame
      currentFrameIndex = (currentFrameIndex + 1) % MAX_FRAMES_IN_FLIGHT
    } catch {
      case e: Exception =>
        e.printStackTrace()
        isRunning = false
    }
  }

  /**
   * Submit a command buffer to the queue with synchronization.
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
        vkQueueSubmit(context.computeQueue.get, submitInfo, fence.get),
        "Failed to submit draw command buffer"
      )
    }
  }

  /**
   * Update frame timing statistics and window title.
   */
  private def updateFrameStats(frameStart: Double): Unit = {
    val now = GLFW.glfwGetTime()
    val frameTime = now - frameStart
    
    // Add to rolling average (limit to last 100 frames)
    frameTimes.append(frameTime)
    if (frameTimes.length > 100) {
      frameTimes.remove(0)
    }
    
    frameCounter += 1
    
    // Update window title with FPS every second
    if (now - fpsUpdateTime >= 1.0) {
      val fps = frameCounter / (now - fpsUpdateTime)
      val frameTimeMs = frameTimes.sum / frameTimes.length * 1000.0
      GLFW.glfwSetWindowTitle(window.nativePtr, 
        s"$windowTitle | FPS: ${fps.toInt} | Frame Time: ${frameTimeMs.toInt} ms")
      
      frameCounter = 0
      fpsUpdateTime = now
    }
  }

  /**
   * Sleep to maintain target frame rate.
   */
  private def applyFramePacing(frameStart: Double): Unit = {
    val targetFrameTime = 1.0 / targetFPS
    val now = GLFW.glfwGetTime()
    val elapsedTime = now - frameStart
    val sleepTime = targetFrameTime - elapsedTime
    
    if (sleepTime > 0.001) { // Only sleep if we have more than 1ms to spare
      val sleepMs = (sleepTime * 1000.0).toLong
      Thread.sleep(sleepMs)
    }
  }

  /**
   * Recreate swap chain and related resources (e.g., after resize).
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
    
    // Wait for device to be idle before recreating resources
    context.device.waitIdle()
    
    // Recreate swap chain
    swapChainManager.initialize(width, height)
    
    // Recreate command buffers if needed
    val swapChainImages = swapChainManager.getImages
    if (commandBuffers == null || commandBuffers.length != swapChainImages.length) {
      // Free old command buffers if they exist
      if (commandBuffers != null) {
        commandPool.freeCommandBuffer(commandBuffers: _*)
      }
      
      // Create new command buffers
      createCommandBuffers()
    }
    
    // Reset the images in flight since we have a new swap chain
    imagesInFlight = Map.empty
  }

  /**
   * Check if all Vulkan resources are initialized.
   */
  def isInitialized: Boolean = {
    window != null && window.nativePtr != 0 && 
    swapChainManager != null && 
    commandBuffers != null && commandBuffers.nonEmpty &&
    imageAvailableSemaphores.nonEmpty && renderFinishedSemaphores.nonEmpty
  }

  /**
   * Release all Vulkan and window resources.
   */
  override def close(): Unit = {
    isRunning = false
    
    if (context != null) {
      context.device.waitIdle()
    }
    
    // Clean up Vulkan resources - synchronization objects
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
    
    // Clean up command buffers
    if (commandBuffers != null && commandPool != null) {
      commandPool.freeCommandBuffer(commandBuffers: _*)
    }
    
    // Clean up swap chain
    if (swapChainManager != null) {
      swapChainManager.close()
    }
    
    // Clean up surface
    if (surfaceManager != null) {
      surfaceManager.destroy()
    }
    
    // Clean up window
    if (window != null && window.nativePtr != 0) {
      windowSystem.destroyWindow(window)
    }
  }
}