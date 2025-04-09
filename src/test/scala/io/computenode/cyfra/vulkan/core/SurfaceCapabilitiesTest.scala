package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext
import org.lwjgl.glfw.GLFW
import org.lwjgl.vulkan.KHRSurface._
import org.lwjgl.vulkan.VK10._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

class SurfaceCapabilitiesTest extends AnyFunSuite with BeforeAndAfterEach {
  private var vulkanContext: VulkanContext = _
  private var windowHandle: Long = _
  private var surface: Surface = _
  private var surfaceCapabilities: SurfaceCapabilities = _

  override def beforeEach(): Unit = {
    assert(GLFW.glfwInit(), "Failed to initialize GLFW")

    GLFW.glfwDefaultWindowHints()
    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)
    GLFW.glfwWindowHint(GLFW.GLFW_VISIBLE, GLFW.GLFW_FALSE)

    windowHandle = GLFW.glfwCreateWindow(800, 600, "Surface Capabilities Test", 0, 0)
    assert(windowHandle != 0, "Failed to create GLFW window")

    vulkanContext = new VulkanContext(true)
    surface = new Surface(vulkanContext.instance, windowHandle)
    surfaceCapabilities = new SurfaceCapabilities(vulkanContext.device.physicalDevice, surface)
  }

  override def afterEach(): Unit = {
    if (surface != null) surface.destroy()
    if (vulkanContext != null) vulkanContext.destroy()
    if (windowHandle != 0) GLFW.glfwDestroyWindow(windowHandle)
    GLFW.glfwTerminate()
  }

  test("Get surface formats") {
    val formats = surfaceCapabilities.getSurfaceFormats()

    assert(formats != null, "Surface formats should not be null")
    assert(formats.nonEmpty, "Should get at least one format")

    formats.foreach { format =>
      assert(format != null, "Format should not be null")
      assert(format.format() >= 0, "Format value should be valid")
      assert(format.colorSpace() >= 0, "Color space value should be valid")
    }
  }

  test("Get presentation modes") {
    val modes = surfaceCapabilities.getPresentationModes()

    assert(modes != null, "Presentation modes should not be null")
    assert(modes.nonEmpty, "Should get at least one presentation mode")
    assert(modes.contains(VK_PRESENT_MODE_FIFO_KHR), "FIFO presentation mode should be supported")
  }

  test("Image count limits") {
    val minCount = surfaceCapabilities.getMinImageCount()
    val maxCount = surfaceCapabilities.getMaxImageCount()

    assert(minCount > 0, "Minimum image count should be positive")
    assert(maxCount == 0 || maxCount >= minCount, "Maximum image count should be 0 (unlimited) or >= minimum")
  }

  test("Extent boundaries") {
    val minExtent = surfaceCapabilities.getMinExtent()
    val maxExtent = surfaceCapabilities.getMaxExtent()

    assert(minExtent._1 > 0 && minExtent._2 > 0, "Minimum extent should be positive")
    assert(maxExtent._1 >= minExtent._1 && maxExtent._2 >= minExtent._2, "Maximum extent should be >= minimum")
  }

  test("Choose surface format") {
    val format = surfaceCapabilities.chooseSurfaceFormat()

    assert(format != null, "Should choose a surface format")

    val specificFormat = surfaceCapabilities.chooseSurfaceFormat(
      VK_FORMAT_B8G8R8A8_UNORM,
      VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
    )

    assert(specificFormat != null, "Should choose a format with specific preferences")
  }

  test("Choose present mode") {
    val mode = surfaceCapabilities.choosePresentMode()

    assert(mode >= 0, "Should choose a valid presentation mode")

    val fifoMode = surfaceCapabilities.choosePresentMode(VK_PRESENT_MODE_FIFO_KHR)
    assert(fifoMode == VK_PRESENT_MODE_FIFO_KHR, "Should choose FIFO mode when requested")
  }

  test("Choose swap extent") {
    val extent = surfaceCapabilities.chooseSwapExtent(1024, 768)

    assert(extent != null, "Should choose a valid extent")
    assert(extent.width() > 0 && extent.height() > 0, "Chosen extent should have positive dimensions")

    val (minW, minH) = surfaceCapabilities.getMinExtent()
    val (maxW, maxH) = surfaceCapabilities.getMaxExtent()

    assert(extent.width() >= minW && extent.width() <= maxW, "Width should be within bounds")
    assert(extent.height() >= minH && extent.height() <= maxH, "Height should be within bounds")
  }
}