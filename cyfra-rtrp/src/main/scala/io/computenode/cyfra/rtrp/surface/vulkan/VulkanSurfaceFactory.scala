package io.computenode.cyfra.rtrp.surface.vulkan

import io.computenode.cyfra.rtrp.surface.core.*
import io.computenode.cyfra.rtrp.surface.*
import io.computenode.cyfra.rtrp.window.core.Window
import io.computenode.cyfra.vulkan.VulkanContext
import org.lwjgl.glfw.GLFWVulkan
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.VK10.*
import scala.util.{Try, Success, Failure}
import java.util.concurrent.atomic.AtomicLong

// Factory for creating Vulkan surfaces from windows
class VulkanSurfaceFactory(vulkanContext: VulkanContext) {

  private val surfaceIdGenerator = new AtomicLong(1L)

  def createSurface(window: Window, config: SurfaceConfig): Try[VulkanSurface] = Try {
    if window.nativeHandle == 0L then throw SurfaceCreationException("Window native handle is null")
    if vulkanContext.instance.get.address() == 0L then throw VulkanSurfaceCreationException("VulkanContext instance is null")

    val surfaceHandle = createVulkanSurface(window.nativeHandle)

    val surfaceId = SurfaceId(surfaceIdGenerator.getAndIncrement())

    val surface = new VulkanSurface(surfaceId, window.id, surfaceHandle, vulkanContext)

    surface.getCapabilities() match {
      case Success(capabilities) =>
        validateSurfaceConfig(surface, config, capabilities)
        surface
      case Failure(ex) =>
        surface.destroy()
        throw new RuntimeException("Failed to validate created surface", ex)
    }
  }

  // Create multiple surfaces for multiple windows.
  def createSurfaces(windows: List[Window], config: SurfaceConfig): Try[List[VulkanSurface]] = {
    val results = windows.map(createSurface(_, config))

    val failures = results.collect { case Failure(ex) => ex }
    if failures.nonEmpty then {
      results.collect { case Success(surface) => surface.destroy() }
      Failure(new RuntimeException(s"Failed to create ${failures.size} surfaces", failures.head))
    } else Success(results.collect { case Success(surface) => surface })
  }

  private def createVulkanSurface(windowHandle: Long): Long = {
    MemoryStack.stackPush()
    try {
      val stack = MemoryStack.stackGet()
      val pSurface = stack.callocLong(1)

      // This is the key GLFW-Vulkan bridge function
      val result = GLFWVulkan.glfwCreateWindowSurface(vulkanContext.instance.get, windowHandle, null, pSurface)

      if result != VK_SUCCESS then throw VulkanSurfaceCreationException(s"Failed to create Vulkan surface: $result")

      val surfaceHandle = pSurface.get(0)

      if surfaceHandle == 0L then throw VulkanSurfaceCreationException("Created surface handle is null")

      surfaceHandle
    } finally MemoryStack.stackPop()
  }

  private def validateSurfaceConfig(surface: VulkanSurface, config: SurfaceConfig, capabilities: SurfaceCapabilities): Unit = {
    if !capabilities.supportsFormat(config.preferredFormat) then
      println(s"Warning: Preferred format ${config.preferredFormat} not supported by surface ${surface.id}")

    if !capabilities.supportsPresentMode(config.preferredPresentMode) then
      println(s"Warning: Preferred present mode ${config.preferredPresentMode} not supported by surface ${surface.id}")

    config.minImageCount.foreach { minCount =>
      if minCount < capabilities.minImageCount then
        println(s"Warning: Requested min image count $minCount is less than supported minimum ${capabilities.minImageCount}")
    }

    config.maxImageCount.foreach { maxCount =>
      if maxCount > capabilities.maxImageCount && capabilities.maxImageCount != Int.MaxValue then
        println(s"Warning: Requested max image count $maxCount exceeds supported maximum ${capabilities.maxImageCount}")
    }
  }
}
