package io.computenode.cyfra.window

import scala.util.{Try, Success, Failure}
import org.lwjgl.glfw.GLFW
import org.lwjgl.system.MemoryUtil

/**
 * Test program for validating the GLFWWindowSystem implementation.
 */
object WindowSystemTest {
  // Window dimensions and title
  private val Width = 800
  private val Height = 600
  private val Title = "GLFW Window System Test"
  
  // Target FPS for the main loop
  private val TargetFPS = 60
  private val FrameTime = 1.0 / TargetFPS
  
  def main(args: Array[String]): Unit = {
    println("Starting Window System Test")
    
    // Create window system in a try block to handle initialization errors
    try {
      val windowSystem = new GLFWWindowSystem()
      var window: WindowHandle = null
      
      try {
        // Create a window
        println(s"Creating window (${Width}x${Height}: $Title)")
        window = windowSystem.createWindow(Width, Height, Title)
        
        // Enter the main event loop
        mainLoop(windowSystem, window)
      } catch {
        case ex: Exception =>
          println(s"Error during window operation: ${ex.getMessage}")
          ex.printStackTrace()
      } finally {
        // Clean up window if it was created
        if (window != null && window.nativePtr != MemoryUtil.NULL) {
          println("Destroying window")
          GLFW.glfwDestroyWindow(window.nativePtr)
        }
        
        // Additional cleanup if needed
        println("Terminating GLFW")
        GLFW.glfwTerminate()
      }
    } catch {
      case ex: Exception =>
        println(s"Error initializing window system: ${ex.getMessage}")
        ex.printStackTrace()
    }
    
    println("Window System Test completed")
  }
  
  /**
   * Main application loop that polls and handles events.
   */
  private def mainLoop(windowSystem: GLFWWindowSystem, window: WindowHandle): Unit = {
    println("Entering main loop")
    
    var lastTime = GLFW.glfwGetTime()
    var frameCount = 0
    var frameTimer = 0.0
    
    // Run until the window should close
    while (!windowSystem.shouldWindowClose(window)) {
      val currentTime = GLFW.glfwGetTime()
      val deltaTime = currentTime - lastTime
      lastTime = currentTime
      
      // Update FPS counter
      frameCount += 1
      frameTimer += deltaTime
      if (frameTimer >= 1.0) {
        println(s"FPS: $frameCount")
        frameCount = 0
        frameTimer = 0.0
      }
      
      // Poll and handle window events
      val events = windowSystem.pollEvents()
      if (events.nonEmpty) {
        println(s"Received ${events.size} events:")
        windowSystem.handleEvents(events, window)
      }
      
      // Simulate rendering (just sleep to maintain frame rate)
      val frameTimeRemaining = FrameTime - (GLFW.glfwGetTime() - currentTime)
      if (frameTimeRemaining > 0) {
        Thread.sleep((frameTimeRemaining * 1000).toLong)
      }
    }
    
    println("Window closed, exiting main loop")
  }
}

// Test this file using - sbt "runMain io.computenode.cyfra.window.WindowSystemTest"