package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext
import org.lwjgl.glfw.GLFW
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

class SurfaceManagerTest extends AnyFunSuite with BeforeAndAfterEach {
  private var vulkanContext: VulkanContext = _
  private var surfaceManager: SurfaceManager = _
  private var windowHandles: Array[Long] = _

  override def beforeEach(): Unit = {
    assert(GLFW.glfwInit(), "Failed to initialize GLFW")

    GLFW.glfwDefaultWindowHints()
    GLFW.glfwWindowHint(GLFW.GLFW_CLIENT_API, GLFW.GLFW_NO_API)
    GLFW.glfwWindowHint(GLFW.GLFW_VISIBLE, GLFW.GLFW_FALSE)

    windowHandles = Array(
      GLFW.glfwCreateWindow(800, 600, "Test Window 1", 0, 0),
      GLFW.glfwCreateWindow(640, 480, "Test Window 2", 0, 0)
    )

    windowHandles.foreach(handle => assert(handle != 0, "Failed to create GLFW window"))

    vulkanContext = new VulkanContext(true)
    surfaceManager = new SurfaceManager(vulkanContext)
  }

  override def afterEach(): Unit = {
    if (surfaceManager != null) surfaceManager.destroy()
    if (vulkanContext != null) vulkanContext.destroy()
    if (windowHandles != null) windowHandles.foreach(handle => if (handle != 0) GLFW.glfwDestroyWindow(handle))
    GLFW.glfwTerminate()
  }

  test("Create and get surface") {
    val surface = surfaceManager.createSurface(windowHandles(0))

    assert(surface != null, "Surface should be created")
    assert(surface.get != 0L, "Surface handle should not be 0")

    val retrievedSurface = surfaceManager.getSurface(windowHandles(0))
    assert(retrievedSurface.isDefined, "Surface should be retrievable")
    assert(retrievedSurface.get.get == surface.get, "Retrieved surface should match created surface")
  }

  test("Multiple surfaces") {
    val surface1 = surfaceManager.createSurface(windowHandles(0))
    val surface2 = surfaceManager.createSurface(windowHandles(1))

    assert(surface1 != null, "Surface 1 should be created")
    assert(surface2 != null, "Surface 2 should be created")

    assert(surfaceManager.getActiveSurfaceCount == 2, "Should track 2 active surfaces")
    assert(surfaceManager.hasSurface(windowHandles(0)), "Should have surface for window 1")
    assert(surfaceManager.hasSurface(windowHandles(1)), "Should have surface for window 2")
  }

  test("Remove surface") {
    surfaceManager.createSurface(windowHandles(0))
    val result = surfaceManager.removeSurface(windowHandles(0))

    assert(result, "Surface removal should succeed")
    assert(!surfaceManager.hasSurface(windowHandles(0)), "Surface should no longer exist")
    assert(surfaceManager.getActiveSurfaceCount == 0, "Should have 0 active surfaces")
  }

  test("Get all surfaces") {
    val surface1 = surfaceManager.createSurface(windowHandles(0))
    val surface2 = surfaceManager.createSurface(windowHandles(1))

    val allSurfaces = surfaceManager.getAllSurfaces

    assert(allSurfaces.size == 2, "Should return 2 surfaces")
    assert(allSurfaces.exists(_.get == surface1.get), "Should contain surface 1")
    assert(allSurfaces.exists(_.get == surface2.get), "Should contain surface 2")
  }

  test("Event handling") {
    var resizeEventReceived = false
    val testManager = new SurfaceManager(vulkanContext) {
      override def onWindowResize(windowHandle: Long, width: Int, height: Int): Unit = {
        resizeEventReceived = true
        super.onWindowResize(windowHandle, width, height)
      }
    }

    testManager.createSurface(windowHandles(0))
    testManager.onWindowResize(windowHandles(0), 1024, 768)

    assert(resizeEventReceived, "Resize event should be processed")
    testManager.destroy()
  }

  test("Window close handling") {
    surfaceManager.createSurface(windowHandles(0))
    surfaceManager.onWindowClose(windowHandles(0))

    assert(!surfaceManager.hasSurface(windowHandles(0)), "Surface should be removed after close")
  }

  test("Get all surface capabilities") {
    surfaceManager.createSurface(windowHandles(0))
    surfaceManager.createSurface(windowHandles(1))

    val allCapabilities = surfaceManager.getAllSurfaceCapabilities

    assert(allCapabilities.size == 2, "Should get capabilities for 2 surfaces")
    assert(allCapabilities.contains(windowHandles(0)), "Should contain capabilities for window 1")
    assert(allCapabilities.contains(windowHandles(1)), "Should contain capabilities for window 2")
  }
}