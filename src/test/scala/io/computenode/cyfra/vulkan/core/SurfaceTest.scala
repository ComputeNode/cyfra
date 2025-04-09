package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext
import org.lwjgl.glfw.{GLFW, GLFWVulkan}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Try

class SurfaceTest extends AnyFunSuite with BeforeAndAfterEach {
  private var vulkanContext: VulkanContext = _
  private var windowHandle: Long = _

  override def beforeEach(): Unit = {
    // Initialize GLFW
    assert(GLFW.glfwInit(), "Failed to initialize GLFW")

    // Configure GLFW window creation
    GLFW.glfwDefaultWindowHints()
    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)
    GLFW.glfwWindowHint(GLFW.GLFW_VISIBLE, GLFW.GLFW_FALSE)

    // Create window
    windowHandle = GLFW.glfwCreateWindow(800, 600, "Surface Test", 0, 0)
    assert(windowHandle != 0, "Failed to create GLFW window")

    // Create Vulkan context with validation
    vulkanContext = new VulkanContext(true)
  }

  override def afterEach(): Unit = {
    // Destroy Vulkan context
    if (vulkanContext != null) vulkanContext.destroy()

    // Destroy window and terminate GLFW
    if (windowHandle != 0) GLFW.glfwDestroyWindow(windowHandle)
    GLFW.glfwTerminate()
  }

  test("Surface creation and destruction") {
    val surface = Try(new Surface(vulkanContext.instance, windowHandle))

    assert(surface.isSuccess, "Surface creation should succeed")
    assert(surface.get.get != 0L, "Surface handle should not be 0")

    surface.get.destroy()
  }

  test("Get surface capabilities") {
    val surface = new Surface(vulkanContext.instance, windowHandle)

    try {
      val capabilities = surface.getCapabilities(vulkanContext.device.physicalDevice)

      assert(capabilities != null, "Surface capabilities should not be null")
      assert(capabilities.minImageCount() > 0, "Minimum image count should be positive")
    } finally {
      surface.destroy()
    }
  }

  test("Supports presentation from compute queue") {
    val surface = new Surface(vulkanContext.instance, windowHandle)

    try {
      val isSupported = surface.supportsPresentationFrom(
        vulkanContext.device.physicalDevice,
        vulkanContext.device.computeQueueFamily
      )

      info(s"Presentation support for compute queue: $isSupported")
    } finally {
      surface.destroy()
    }
  }
}