package io.computenode.cyfra.rtrp.surface.vulkan

import io.computenode.cyfra.rtrp.surface.core.*
import io.computenode.cyfra.vulkan.VulkanContext
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.*
import scala.jdk.CollectionConverters.*
import scala.util.*

// Vulkan-specific surface capabilities implementation
class VulkanSurfaceCapabilities(vulkanContext: VulkanContext, surface: VulkanSurface) extends SurfaceCapabilities:

  // Query Vulkan for capabilities
  private val vkCapabilities = queryVulkanCapabilities()
  private val vkFormats = queryAvailableFormats()
  private val vkPresentModes = queryPresentModes()

  override def supportedFormats: List[SurfaceFormat] =
    vkFormats.map(convertVulkanFormat).distinct

  override def supportedColorSpaces: List[ColorSpace] =
    vkFormats.map(convertVulkanColorSpace).distinct

  override def supportedPresentModes: List[PresentMode] =
    vkPresentModes.map(convertVulkanPresentMode)

  override def minImageExtent: (Int, Int) =
    (vkCapabilities.minImageExtent().width(), vkCapabilities.minImageExtent().height())

  override def maxImageExtent: (Int, Int) =
    (vkCapabilities.maxImageExtent().width(), vkCapabilities.maxImageExtent().height())

  override def currentExtent: (Int, Int) =
    val extent = vkCapabilities.currentExtent()
    // If width/height is 0xFFFFFFFF, then surface size will be determined by the swapchain
    if extent.width() == 0xffffffff || extent.height() == 0xffffffff then
      // Return a reasonable default
      (800, 600)
    else (extent.width(), extent.height())

  override def minImageCount: Int = vkCapabilities.minImageCount()

  override def maxImageCount: Int =
    val max = vkCapabilities.maxImageCount()
    if max == 0 then Int.MaxValue else max // 0 means no limit

  override def supportsAlpha: Boolean = (vkCapabilities.supportedCompositeAlpha() & VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR) != 0

  override def supportsTransform: Boolean = (vkCapabilities.supportedTransforms() & VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR) != 0

  // Private methods to query Vulkan

  private def queryVulkanCapabilities(): VkSurfaceCapabilitiesKHR =
    MemoryStack.stackPush()
    try
      val stack = MemoryStack.stackGet()
      val capabilities = VkSurfaceCapabilitiesKHR.callocStack(stack)

      val result = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(vulkanContext.device.physicalDevice, surface.nativeHandle, capabilities)

      if result != VK_SUCCESS then throw new RuntimeException(s"Failed to get surface capabilities: $result")

      capabilities
    finally MemoryStack.stackPop()

  private def queryAvailableFormats(): List[VkSurfaceFormatKHR] =
    MemoryStack.stackPush()
    try
      val stack = MemoryStack.stackGet()
      val pFormatCount = stack.callocInt(1)

      // First call: get count
      vkGetPhysicalDeviceSurfaceFormatsKHR(vulkanContext.device.physicalDevice, surface.nativeHandle, pFormatCount, null)

      val formatCount = pFormatCount.get(0)
      if formatCount == 0 then return List.empty

      val formats = VkSurfaceFormatKHR.callocStack(formatCount, stack)

      // Second call: get actual formats
      vkGetPhysicalDeviceSurfaceFormatsKHR(vulkanContext.device.physicalDevice, surface.nativeHandle, pFormatCount, formats)

      (0 until formatCount).map(formats.get).toList
    finally MemoryStack.stackPop()

  private def queryPresentModes(): List[Int] =
    MemoryStack.stackPush()
    try
      val stack = MemoryStack.stackGet()
      val pModeCount = stack.callocInt(1)

      // First call: get count
      vkGetPhysicalDeviceSurfacePresentModesKHR(vulkanContext.device.physicalDevice, surface.nativeHandle, pModeCount, null)

      val modeCount = pModeCount.get(0)
      if modeCount == 0 then return List.empty

      val modes = stack.callocInt(modeCount)

      // Second call: get actual modes
      vkGetPhysicalDeviceSurfacePresentModesKHR(vulkanContext.device.physicalDevice, surface.nativeHandle, pModeCount, modes)

      (0 until modeCount).map(modes.get).toList
    finally MemoryStack.stackPop()

  // Conversion methods

  private def convertVulkanFormat(vkFormat: VkSurfaceFormatKHR): SurfaceFormat =
    vkFormat.format() match
      case VK_FORMAT_B8G8R8A8_SRGB  => SurfaceFormat.B8G8R8A8_SRGB
      case VK_FORMAT_B8G8R8A8_UNORM => SurfaceFormat.B8G8R8A8_UNORM
      case VK_FORMAT_R8G8B8A8_SRGB  => SurfaceFormat.R8G8B8A8_SRGB
      case VK_FORMAT_R8G8B8A8_UNORM => SurfaceFormat.R8G8B8A8_UNORM
      case _                        => SurfaceFormat.B8G8R8A8_SRGB // Default fallback

  private def convertVulkanColorSpace(vkFormat: VkSurfaceFormatKHR): ColorSpace =
    vkFormat.colorSpace() match
      case VK_COLOR_SPACE_SRGB_NONLINEAR_KHR => ColorSpace.SRGB_NONLINEAR
      case 1000104001                        => ColorSpace.DISPLAY_P3_NONLINEAR // VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
      case _                                 => ColorSpace.SRGB_NONLINEAR // Default fallback

  private def convertVulkanPresentMode(vkMode: Int): PresentMode =
    vkMode match
      case VK_PRESENT_MODE_IMMEDIATE_KHR    => PresentMode.IMMEDIATE
      case VK_PRESENT_MODE_MAILBOX_KHR      => PresentMode.MAILBOX
      case VK_PRESENT_MODE_FIFO_KHR         => PresentMode.FIFO
      case VK_PRESENT_MODE_FIFO_RELAXED_KHR => PresentMode.FIFO_RELAXED
      case _                                => PresentMode.FIFO // Default fallback
