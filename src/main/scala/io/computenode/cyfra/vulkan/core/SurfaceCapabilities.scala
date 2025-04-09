package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkExtent2D, VkPhysicalDevice, VkSurfaceCapabilitiesKHR, VkSurfaceFormatKHR}

import scala.jdk.CollectionConverters.given
import collection.mutable.ArrayBuffer

/** Class that encapsulates Vulkan surface capabilities (VkSurfaceCapabilitiesKHR)
  * and provides properties for presentation modes, formats, image count, etc.
  *
  * @param physicalDevice The physical device to query capabilities for
  * @param surface The surface to query capabilities for
  */
private[cyfra] class SurfaceCapabilities(physicalDevice: VkPhysicalDevice, surface: Surface) {

  /** Get the surface capabilities */
  def getCapabilities(): VkSurfaceCapabilitiesKHR = pushStack { stack =>
    val capabilities = VkSurfaceCapabilitiesKHR.calloc(stack)
    check(
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice, surface.get, capabilities),
      "Failed to get surface capabilities"
    )
    capabilities
  }
  
  /** Get the supported surface formats */
  def getSurfaceFormats(): Seq[VkSurfaceFormatKHR] = pushStack { stack =>
    val countPtr = stack.callocInt(1)
    check(
      vkGetPhysicalDeviceSurfaceFormatsKHR(physicalDevice, surface.get, countPtr, null),
      "Failed to get surface format count"
    )
    
    val count = countPtr.get(0)
    if (count == 0) {
      return Seq.empty
    }
    
    val surfaceFormats = VkSurfaceFormatKHR.calloc(count, stack)
    check(
      vkGetPhysicalDeviceSurfaceFormatsKHR(physicalDevice, surface.get, countPtr, surfaceFormats),
      "Failed to get surface formats"
    )
    
    surfaceFormats.iterator().asScala.toSeq
  }
  
  /** Get the supported presentation modes */
  def getPresentationModes(): Seq[Int] = pushStack { stack =>
    val countPtr = stack.callocInt(1)
    check(
      vkGetPhysicalDeviceSurfacePresentModesKHR(physicalDevice, surface.get, countPtr, null),
      "Failed to get presentation mode count"
    )
    
    val count = countPtr.get(0)
    if (count == 0) {
      return Seq.empty
    }
    
    val presentModes = stack.callocInt(count)
    check(
      vkGetPhysicalDeviceSurfacePresentModesKHR(physicalDevice, surface.get, countPtr, presentModes),
      "Failed to get presentation modes"
    )
    
    val result = ArrayBuffer[Int]()
    for (i <- 0 until count) {
      result += presentModes.get(i)
    }
    result.toSeq
  }
  
  /** Get the minimum image count supported */
  def getMinImageCount(): Int = getCapabilities().minImageCount()
  
  /** Get the maximum image count supported (0 means no limit) */
  def getMaxImageCount(): Int = getCapabilities().maxImageCount()
  
  /** Get the current surface extent */
  def getCurrentExtent(): (Int, Int) = {
    val extent = getCapabilities().currentExtent()
    (extent.width(), extent.height())
  }
  
  /** Get the minimum surface extent */
  def getMinExtent(): (Int, Int) = {
    val extent = getCapabilities().minImageExtent()
    (extent.width(), extent.height())
  }
  
  /** Get the maximum surface extent */
  def getMaxExtent(): (Int, Int) = {
    val extent = getCapabilities().maxImageExtent()
    (extent.width(), extent.height())
  }
  
  /** Check if a specific format is supported */
  def isFormatSupported(format: Int, colorSpace: Int): Boolean = {
    getSurfaceFormats().exists(sf => sf.format() == format && sf.colorSpace() == colorSpace)
  }
  
  /** Check if a specific presentation mode is supported */
  def isPresentModeSupported(presentMode: Int): Boolean = {
    getPresentationModes().contains(presentMode)
  }
  
  /** Get the supported transforms */
  def getSupportedTransforms(): Int = getCapabilities().supportedTransforms()
  
  /** Get the current transform */
  def getCurrentTransform(): Int = getCapabilities().currentTransform()
  
  /** Check if a specific transform is supported */
  def isTransformSupported(transform: Int): Boolean = {
    (getSupportedTransforms() & transform) != 0
  }
  
  /** Choose an optimal surface format from the available formats
    * 
    * @param preferredFormat The preferred format
    * @param preferredColorSpace The preferred color space
    * @return The selected format or the first available format if the preferred one isn't available
    */
  def chooseSurfaceFormat(preferredFormat: Int = VK_FORMAT_B8G8R8A8_SRGB, 
                         preferredColorSpace: Int = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR): VkSurfaceFormatKHR = {
    val formats = getSurfaceFormats()
    
    // First look for exact match
    formats.find(format => 
      format.format() == preferredFormat && format.colorSpace() == preferredColorSpace
    ).getOrElse {
      // Then look for format with any color space
      formats.find(_.format() == preferredFormat)
        .getOrElse(formats.head) // Just return the first format if no match
    }
  }
  
  /** Choose an optimal presentation mode from the available modes
    * 
    * @param preferredMode The preferred presentation mode
    * @return The selected presentation mode, or VK_PRESENT_MODE_FIFO_KHR if preferred isn't available
    */
  def choosePresentMode(preferredMode: Int = VK_PRESENT_MODE_MAILBOX_KHR): Int = {
    val modes = getPresentationModes()
    
    // If the preferred mode is supported, use it
    if (modes.contains(preferredMode)) {
      preferredMode
    } else {
      // FIFO is guaranteed to be supported
      VK_PRESENT_MODE_FIFO_KHR
    }
  }
  
  /** Choose an appropriate swap extent
    * 
    * @param width The window width
    * @param height The window height
    * @return The selected extent that fits within min/max bounds
    */
  def chooseSwapExtent(width: Int, height: Int): VkExtent2D = pushStack { stack =>
    val capabilities = getCapabilities()
    
    // If currentExtent is set to max uint32, the window manager allows us to set our own resolution
    if (capabilities.currentExtent().width() != Int.MaxValue) {
      return capabilities.currentExtent()
    }
    
    val extent = VkExtent2D.calloc(stack)
      .width(width)
      .height(height)
    
    // Clamp the extent between min and max
    val minExtent = capabilities.minImageExtent()
    val maxExtent = capabilities.maxImageExtent()
    
    extent.width(
      Math.max(minExtent.width(), Math.min(maxExtent.width(), extent.width()))
    )
    
    extent.height(
      Math.max(minExtent.height(), Math.min(maxExtent.height(), extent.height()))
    )
    
    extent
  }
}