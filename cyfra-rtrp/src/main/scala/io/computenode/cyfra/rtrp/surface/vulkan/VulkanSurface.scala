package io.computenode.cyfra.rtrp.surface.vulkan

import io.computenode.cyfra.rtrp.surface.core.*
import io.computenode.cyfra.rtrp.surface.*
import io.computenode.cyfra.rtrp.window.core.WindowId
import io.computenode.cyfra.vulkan.VulkanContext
import org.lwjgl.vulkan.VkInstance
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.VK10.*
import scala.util.*
import java.util.concurrent.atomic.AtomicBoolean

// Vulkan implementation of Surface
class VulkanSurface(val id: SurfaceId, val windowId: WindowId, val nativeHandle: Long, private val vulkanContext: VulkanContext) extends Surface:

  private val destroyed = new AtomicBoolean(false)
  private var surfaceCapabilities: Option[VulkanSurfaceCapabilities] = None
  private var lastKnownSize: Option[(Int, Int)] = None

  override def isValid: Boolean = !destroyed.get() && nativeHandle != 0L

  override def resize(width: Int, height: Int): Try[Unit] = Try:
    checkValid()

    lastKnownSize = Some((width, height))

    surfaceCapabilities = None

    // Note: When a window resizes, we only need to update our tracking info here.
    // The actual Vulkan surface (connection to the window) stays the same.
    // However, the swapchain (which contains the actual images we render to)
    // will need to be recreated with the new dimensions - that happens elsewhere
    // in the swapchain manager when it detects the size change.

  override def getCapabilities(): Try[SurfaceCapabilities] = Try:
    checkValid()

    surfaceCapabilities match
      case Some(caps) => caps
      case None       =>
        val caps = new VulkanSurfaceCapabilities(vulkanContext, this)
        surfaceCapabilities = Some(caps)
        caps

  override def currentSize: Try[(Int, Int)] = Try:
    checkValid()

    lastKnownSize match
      case Some(size) => size
      case None       =>
        getCapabilities().map(_.currentExtent).getOrElse((800, 600))

  override def destroy(): Try[Unit] = Try:
    if !destroyed.getAndSet(true) then
      try vkDestroySurfaceKHR(vulkanContext.instance.get, nativeHandle, null)
      finally
        surfaceCapabilities = None
        lastKnownSize = None

  override def recreate(): Try[Unit] = Try:
    checkValid()

    // Clear cached capabilities to force refresh
    surfaceCapabilities = None

    // Trigger capabilities refresh
    getCapabilities()

  def getInstance: VkInstance = vulkanContext.instance.get

  def getPhysicalDevice = vulkanContext.device.physicalDevice

// Check if this surface supports presentation on the given queue family
  def supportsPresentationOnQueueFamily(queueFamilyIndex: Int): Try[Boolean] = Try:
    checkValid()

    val stack = org.lwjgl.system.MemoryStack.stackPush()
    try
      val pSupported = stack.callocInt(1)

      val result = vkGetPhysicalDeviceSurfaceSupportKHR(vulkanContext.device.physicalDevice, queueFamilyIndex, nativeHandle, pSupported)

      if result != VK_SUCCESS then throw new RuntimeException(s"Failed to check surface support: $result")

      pSupported.get(0) == VK_TRUE
    finally org.lwjgl.system.MemoryStack.stackPop()

  private def checkValid(): Unit =
    if !isValid then throw SurfaceInvalidException("Surface is not valid or has been destroyed")
