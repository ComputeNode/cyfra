package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext
import org.lwjgl.glfw.{GLFW, GLFWVulkan}
import org.lwjgl.system.Platform
import org.lwjgl.vulkan.KHRSurface.VK_KHR_SURFACE_EXTENSION_NAME
import org.lwjgl.vulkan.VK10._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Try

class SurfaceFactoryTest extends AnyFunSuite with BeforeAndAfterEach {
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
    windowHandle = GLFW.glfwCreateWindow(800, 600, "Surface Factory Test", 0, 0)
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

  test("Create surface") {
    val surface = Try(SurfaceFactory.create(vulkanContext, windowHandle))

    assert(surface.isSuccess, "Surface creation should succeed")
    assert(surface.get.get != 0L, "Surface handle should not be 0")

    surface.get.destroy()
  }

  test("Get platform-specific extensions") {
    val extensions = SurfaceFactory.getPlatformSpecificExtensions()

    assert(extensions.nonEmpty, "Should return at least one extension")
    assert(extensions.contains(VK_KHR_SURFACE_EXTENSION_NAME), "Should include KHR_SURFACE extension")

    val platformExtensionFound = Platform.get() match {
      case Platform.WINDOWS =>
        extensions.exists(_.contains("win32"))
      case Platform.LINUX =>
        extensions.exists(ext => ext.contains("xcb") || ext.contains("xlib") || ext.contains("wayland"))
      case Platform.MACOSX =>
        extensions.exists(_.contains("metal"))
      case _ => true // Skip check for unsupported platforms
    }

    assert(platformExtensionFound, "Should include platform-specific extension")
  }

  test("Platform-specific surfaces (disabled)") {
    info("Platform-specific surface creation tests are disabled")
  }
}