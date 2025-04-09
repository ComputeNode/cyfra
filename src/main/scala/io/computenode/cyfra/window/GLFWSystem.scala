package io.computenode.cyfra.window

import org.lwjgl.glfw.{GLFW, GLFWErrorCallback}
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.glfw.GLFWVulkan.glfwVulkanSupported

import scala.util.{Try, Success, Failure}

/**
 * GLFW window system implementation.
 */
object GLFWSystem {
  /**
   * Initializes GLFW with appropriate configuration for Vulkan rendering.
   *
   * @return Success if initialization was successful, Failure otherwise
   */
  def initializeGLFW(): Try[Unit] = Try {
    // Setup error callback
    val errorCallback = GLFWErrorCallback.createPrint(System.err)
    GLFW.glfwSetErrorCallback(errorCallback)
    
    // Initialize GLFW
    if (!GLFW.glfwInit()) {
      throw new RuntimeException("Failed to initialize GLFW")
    }
    
    // Configure GLFW for Vulkan
    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)  // No OpenGL context
    GLFW.glfwWindowHint(GLFW.GLFW_RESIZABLE, GLFW.GLFW_TRUE)     // Window resizable
    
    // Check Vulkan support
    if (!glfwVulkanSupported()) {
      throw new RuntimeException("GLFW: Vulkan is not supported")
    }
    
    // Register shutdown hook to terminate GLFW
    sys.addShutdownHook {
      GLFW.glfwTerminate()
      if (errorCallback != null) errorCallback.free()
    }
  }
}