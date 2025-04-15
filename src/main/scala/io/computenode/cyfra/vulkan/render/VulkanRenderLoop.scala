package io.computenode.cyfra.vulkan.render

import io.computenode.cyfra.dsl.RGBA
import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.command.CommandPool
import io.computenode.cyfra.vulkan.core.{Surface, SurfaceManager, SwapChainManager}
import io.computenode.cyfra.window.{GLFWWindowSystem, WindowEvent, WindowHandle}
import org.lwjgl.glfw.GLFW
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.*

import java.lang.Thread
import scala.collection.mutable.ArrayBuffer

/**
 * Main Vulkan render loop with proper synchronization, swap chain management,
 * and frame pacing.
 */
class VulkanRenderLoop(windowSystem: GLFWWindowSystem, context: VulkanContext) extends AutoCloseable {
  // Vulkan components
  private var window: WindowHandle = _
  private var surfaceManager: SurfaceManager = _
  private var surface: Surface = _
  private var swapChainManager: SwapChainManager = _
  private val commandPool = context.commandPool
  
  // Synchronization
  private val synchronizer = new RenderLoopSynchronizer(context)
  
  // Frame timing
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
  private var swapChainNeedsRecreation = false
  
  // Command buffers - one per swap chain image
  private var commandBuffers: Array[org.lwjgl.vulkan.VkCommandBuffer] = _
  
  /**
   * Initialize the window and Vulkan resources
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
    val swapChainImages = swapChainManager.getImages
    commandBuffers = new Array[org.lwjgl.vulkan.VkCommandBuffer](swapChainImages.length)
    for (i <- 0 until swapChainImages.length) {
      commandBuffers(i) = commandPool.createCommandBuffer()
    }
    
    // Initialize synchronization
    synchronizer.initialize()
    
    true
  }
  
  /**
   * Set the target FPS for frame pacing
   */
  def setTargetFPS(fps: Int): Unit = {
    targetFPS = fps
  }
  
  /**
   * Enable or disable frame rate limiting
   */
  def setLimitFrameRate(limit: Boolean): Unit = {
    limitFrameRate = limit
  }
  
  /**
   * Get current FPS
   */
  def getCurrentFPS: Double = {
    if (frameTimes.isEmpty) 0.0
    else frameTimes.length / frameTimes.sum
  }
  
  /**
   * Main render loop
   * 
   * @param renderFunction Function to record render commands for a given frame
   *                       (imageIndex, commandBuffer) => Unit
   */
  def run(renderFunction: (Int, org.lwjgl.vulkan.VkCommandBuffer) => Unit): Unit = {
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
      
      // Recreate swap chain if needed (e.g., after resize)
      if (swapChainNeedsRecreation) {
        recreateSwapChain()
        swapChainNeedsRecreation = false
      }
      
      // Render a frame
      renderFrame(renderFunction)
      
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
   * Process window events
   */
  private def handleEvents(): Unit = {
    val events = windowSystem.pollEvents()
    
    for (event <- events) {
      event match {
        case WindowEvent.Resize(newWidth, newHeight) =>
          if (newWidth > 0 && newHeight > 0 && (width != newWidth || height != newHeight)) {
            width = newWidth
            height = newHeight
            swapChainNeedsRecreation = true
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
   * Recreate the swap chain (e.g., after window resize)
   */
  private def recreateSwapChain(): Unit = {
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
      commandBuffers = new Array[org.lwjgl.vulkan.VkCommandBuffer](swapChainImages.length)
      for (i <- 0 until swapChainImages.length) {
        commandBuffers(i) = commandPool.createCommandBuffer()
      }
    }
  }
  
  /**
   * Render a single frame with proper synchronization
   */
  private def renderFrame(renderFunction: (Int, org.lwjgl.vulkan.VkCommandBuffer) => Unit): Unit = {
    // Begin frame - waits for fence and acquires image
    val imageIndex = synchronizer.beginFrame(swapChainManager)
    
    // If image acquisition failed, recreate swap chain
    if (imageIndex < 0) {
      swapChainNeedsRecreation = true
      return
    }
    
    // Get command buffer for this image
    val commandBuffer = commandBuffers(imageIndex)
    
    // Reset command buffer and begin recording
    vkResetCommandBuffer(commandBuffer, 0)
    
    // Record commands using the provided render function
    renderFunction(imageIndex, commandBuffer)
    
    // Submit command buffer with synchronization
    synchronizer.submitCommandBuffer(commandBuffer, imageIndex)
    
    // Present the rendered image
    val presentSuccess = synchronizer.presentFrame(swapChainManager, imageIndex, commandBuffer, context.computeQueue.get)
    if (!presentSuccess) {
      swapChainNeedsRecreation = true
    }
  }
  
  /**
   * Update frame time statistics
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
   * Implement frame pacing to maintain target frame rate
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
   * Check if rendering resources are initialized
   */
  def isInitialized: Boolean = {
    window != null && window.nativePtr != 0 && 
    swapChainManager != null && 
    commandBuffers != null && commandBuffers.nonEmpty
  }
  
  /**
   * Clean up all resources
   */
  override def close(): Unit = {
    isRunning = false
    
    if (context != null) {
      context.device.waitIdle()
    }
    
    // Clean up Vulkan resources
    if (synchronizer != null) {
      synchronizer.close()
    }
    
    if (commandBuffers != null && commandPool != null) {
      commandPool.freeCommandBuffer(commandBuffers: _*)
    }
    
    if (swapChainManager != null) {
      swapChainManager.close()
    }
    
    if (surfaceManager != null) {
      surfaceManager.destroy()
    }
    
    // Clean up window
    if (window != null && window.nativePtr != 0) {
      windowSystem.destroyWindow(window)
    }
  }
}