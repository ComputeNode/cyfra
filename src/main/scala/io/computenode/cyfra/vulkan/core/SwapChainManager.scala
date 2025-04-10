package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.KHRSwapchain.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkExtent2D, VkSwapchainCreateInfoKHR, VkImageViewCreateInfo, VkSurfaceFormatKHR}

import scala.jdk.CollectionConverters.given
import scala.collection.mutable.ArrayBuffer

/**
 * Manages Vulkan swapchain creation and lifecycle
 * 
 * @param context The Vulkan context providing access to device and queues
 * @param surface The surface to create swapchain for
 */
private[cyfra] class SwapChainManager(context: VulkanContext, surface: Surface) extends VulkanObjectHandle {
  private val device = context.device
  private val physicalDevice = device.physicalDevice
  
  // Maximum number of frames being processed concurrently
  private val MAX_FRAMES_IN_FLIGHT = 2
  
  private var swapChainExtent: VkExtent2D = _
  private var swapChainImageFormat: Int = _
  private var swapChainColorSpace: Int = _
  private var swapChainPresentMode: Int = _
  private var swapChainImageCount: Int = _
  private var swapChainImages: Array[Long] = _
  private var swapChainImageViews: Array[Long] = _
  
  protected val handle: Long = VK_NULL_HANDLE
  
  // Add a separate mutable field to track the actual swap chain handle
  private var swapChainHandle: Long = VK_NULL_HANDLE
  
  // Override get to return the actual handle
  override def get: Long = {
    if (!alive)
      throw new IllegalStateException()
    
    // Return the current swap chain handle instead of the immutable 'handle'
    swapChainHandle
  }
  
  /**
   * Choose an optimal surface format for the swap chain
   *
   * @param preferredFormat The preferred format, defaults to VK_FORMAT_B8G8R8A8_SRGB
   * @param preferredColorSpace The preferred color space, defaults to VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
   * @return The selected surface format
   */
  def chooseSwapSurfaceFormat(
    preferredFormat: Int = VK_FORMAT_B8G8R8A8_SRGB, 
    preferredColorSpace: Int = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  ): VkSurfaceFormatKHR = {
    // Query available formats using VulkanContext
    val availableFormats = context.getSurfaceFormats(surface)
    
    if (availableFormats.isEmpty) {
      throw new VulkanAssertionError("No surface formats available", -1)
    }
    
    // First look for exact match of format and color space
    availableFormats.find(format => 
      format.format() == preferredFormat && format.colorSpace() == preferredColorSpace
    ).getOrElse {
      // Then look for format with any color space
      availableFormats.find(_.format() == preferredFormat)
        .getOrElse(availableFormats.head) // Just return the first format if no match
    }
  }
  
  /**
   * Choose an optimal presentation mode for the swap chain
   *
   * @param preferredMode The preferred presentation mode, defaults to VK_PRESENT_MODE_MAILBOX_KHR
   * @return The selected presentation mode, or VK_PRESENT_MODE_FIFO_KHR if preferred isn't available
   */
  def chooseSwapPresentMode(preferredMode: Int = VK_PRESENT_MODE_MAILBOX_KHR): Int = {
    // Query available presentation modes using VulkanContext
    val availableModes = context.getPresentModes(surface)
    
    // If the preferred mode is supported, use it
    if (availableModes.contains(preferredMode)) {
      preferredMode
    } else {
      // FIFO is guaranteed to be supported by the Vulkan spec
      VK_PRESENT_MODE_FIFO_KHR
    }
  }
  
  /**
   * Choose an optimal swap extent based on the window dimensions
   * and surface capabilities.
   *
   * @param width The window width
   * @param height The window height
   * @return The selected extent that fits within min/max bounds
   */
  def chooseSwapExtent(width: Int, height: Int): VkExtent2D = pushStack { stack =>
    val capabilities = context.getSurfaceCapabilities(surface)
    
    // Get current extent from capabilities
    val currentExtent = capabilities.getCurrentExtent()
    
    // If currentExtent is set to max uint32 (0xFFFFFFFF), the window manager 
    // allows us to set our own resolution
    if (currentExtent._1 != Int.MaxValue && currentExtent._2 != Int.MaxValue) {
      val extent = VkExtent2D.calloc(stack)
        .width(currentExtent._1)
        .height(currentExtent._2)
      return extent
    }
    
    val extent = VkExtent2D.calloc(stack)
      .width(width)
      .height(height)
    
    // Clamp the extent between min and max
    val minExtent = capabilities.getMinExtent()
    val maxExtent = capabilities.getMaxExtent()
    
    extent.width(
      Math.max(minExtent._1, Math.min(maxExtent._1, extent.width()))
    )
    
    extent.height(
      Math.max(minExtent._2, Math.min(maxExtent._2, extent.height()))
    )
    
    extent
  }
  
  /**
   * Determine the optimal number of images in the swap chain
   * 
   * @param capabilities The surface capabilities
   * @return The number of images to use
   */
  def determineImageCount(capabilities: SurfaceCapabilities): Int = {
    // Start with minimum + 1 for triple buffering
    var imageCount = capabilities.getMinImageCount() + 1
    
    // If max count is 0, there is no limit
    // Otherwise ensure we don't exceed the maximum
    val maxImageCount = capabilities.getMaxImageCount()
    if (maxImageCount > 0) {
      imageCount = Math.min(imageCount, maxImageCount)
    }
    
    // Make sure we have at least enough images to handle MAX_FRAMES_IN_FLIGHT
    imageCount = Math.max(imageCount, MAX_FRAMES_IN_FLIGHT)
    
    // Double-check against maxImageCount again if we adjusted for MAX_FRAMES_IN_FLIGHT
    if (maxImageCount > 0) {
      imageCount = Math.min(imageCount, maxImageCount)
    }
    
    imageCount
  }
  
  /**
   * Initialize the swap chain with specified dimensions
   * 
   * @param width The current window width
   * @param height The current window height
   * @return True if initialization was successful
   */
  def initialize(width: Int, height: Int): Boolean = pushStack { stack =>
    // Clean up previous swap chain if it exists
    cleanup()
    
    // Get surface capabilities
    val capabilities = context.getSurfaceCapabilities(surface)
    
    // Select swap chain settings using our methods
    val surfaceFormat = chooseSwapSurfaceFormat()
    val presentMode = chooseSwapPresentMode()
    val extent = chooseSwapExtent(width, height)
    
    // Store selected format, extent, and present mode
    swapChainExtent = extent
    swapChainImageFormat = surfaceFormat.format()
    swapChainColorSpace = surfaceFormat.colorSpace()
    swapChainPresentMode = presentMode
    
    // Determine optimal image count based on capabilities and MAX_FRAMES_IN_FLIGHT
    val imageCount = determineImageCount(capabilities)
    swapChainImageCount = imageCount
    
    // Check if the compute queue supports presentation
    val queueFamilyIndex = device.computeQueueFamily
    val supportsPresentation = context.isQueueFamilyPresentSupported(queueFamilyIndex, surface)
    
    // Use exclusive mode since we're only using one queue
    val queueFamilyIndices = Array(queueFamilyIndex)
    
    // Create the swap chain
    val createInfo = VkSwapchainCreateInfoKHR.calloc(stack)
      .sType$Default()
      .surface(surface.get)
      .minImageCount(imageCount)
      .imageFormat(surfaceFormat.format())
      .imageColorSpace(surfaceFormat.colorSpace())
      .imageExtent(extent)
      .imageArrayLayers(1) // Always 1 unless using stereoscopic 3D
      .imageUsage(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
      .preTransform(capabilities.getCurrentTransform()) // Use current transform
      .compositeAlpha(VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
      .presentMode(presentMode)
      .clipped(true)  // Ignore obscured pixels
      .oldSwapchain(VK_NULL_HANDLE)
    
    // Set image sharing mode based on queue usage
    // Exclusive mode offers better performance
    createInfo.imageSharingMode(VK_SHARING_MODE_EXCLUSIVE)
      .queueFamilyIndexCount(0)
      .pQueueFamilyIndices(null)
    
    // Create the swap chain
    val pSwapChain = stack.callocLong(1)
    val result = vkCreateSwapchainKHR(device.get, createInfo, null, pSwapChain)
    if (result != VK_SUCCESS) {
      throw new VulkanAssertionError("Failed to create swap chain", result)
    }
    swapChainHandle = pSwapChain.get(0)  // Store in the mutable field
    
    // Get the swap chain images
    val pImageCount = stack.callocInt(1)
    vkGetSwapchainImagesKHR(device.get, swapChainHandle, pImageCount, null)
    val actualImageCount = pImageCount.get(0)
    
    val pSwapChainImages = stack.callocLong(actualImageCount)
    vkGetSwapchainImagesKHR(device.get, swapChainHandle, pImageCount, pSwapChainImages)
    
    swapChainImages = new Array[Long](actualImageCount)
    for (i <- 0 until actualImageCount) {
      swapChainImages(i) = pSwapChainImages.get(i)
    }
    
    // Create image views for the swap chain images
    createImageViews()
    
    true
  }
  
  /**
   * Create image views for the swap chain images
   */
  private def createImageViews(): Unit = pushStack { stack =>
    if (swapChainImages == null || swapChainImages.isEmpty) {
      throw new VulkanAssertionError("Cannot create image views: swap chain images not initialized", -1)
    }
    
    // Clean up previous image views if they exist
    if (swapChainImageViews != null) {
      swapChainImageViews.foreach(imageView => 
        if (imageView != VK_NULL_HANDLE) {
          vkDestroyImageView(device.get, imageView, null)
        }
      )
    }
    
    swapChainImageViews = new Array[Long](swapChainImages.length)
    
    for (i <- swapChainImages.indices) {
      // Configure the image view creation info
      val createInfo = VkImageViewCreateInfo.calloc(stack)
        .sType$Default()
        .image(swapChainImages(i))
        .viewType(VK_IMAGE_VIEW_TYPE_2D)  // 2D texture images
        .format(swapChainImageFormat)     // Same format as the swap chain images
        
      // Component mapping - use identity swizzle (no remapping)
      createInfo.components { components =>
        components
          .r(VK_COMPONENT_SWIZZLE_IDENTITY)
          .g(VK_COMPONENT_SWIZZLE_IDENTITY)
          .b(VK_COMPONENT_SWIZZLE_IDENTITY)
          .a(VK_COMPONENT_SWIZZLE_IDENTITY)
      }
      
      // Define subresource range - for color images without mipmapping or multiple layers
      createInfo.subresourceRange { range =>
        range
          .aspectMask(VK_IMAGE_ASPECT_COLOR_BIT)  // Color aspect only
          .baseMipLevel(0)                       // Start at first mip level
          .levelCount(1)                         // Only one mip level
          .baseArrayLayer(0)                     // Start at first array layer
          .layerCount(1)                         // Only one array layer
      }
      
      // Create the image view
      val pImageView = stack.callocLong(1)
      check(
        vkCreateImageView(device.get, createInfo, null, pImageView),
        s"Failed to create image view for swap chain image $i"
      )
      swapChainImageViews(i) = pImageView.get(0)
    }
  }
  
  /**
   * Get the swap chain extent
   * 
   * @return The current swap chain extent
   */
  def getExtent: VkExtent2D = swapChainExtent
  
  /**
   * Get the swap chain image format
   * 
   * @return The current image format
   */
  def getImageFormat: Int = swapChainImageFormat
  
  /**
   * Get the swap chain images
   * 
   * @return Array of image handles (VkImage)
   */
  def getImages: Array[Long] = swapChainImages
  
  /**
   * Get the swap chain image views
   * 
   * @return Array of image view handles
   */
  def getImageViews: Array[Long] = swapChainImageViews
  
  /**
   * Clean up any existing swap chain resources
   */
  private def cleanup(): Unit = {
    if (swapChainImageViews != null) {
      swapChainImageViews.foreach(imageView => 
        if (imageView != VK_NULL_HANDLE) {
          vkDestroyImageView(device.get, imageView, null)
        }
      )
      swapChainImageViews = null
    }
    
    if (swapChainHandle != VK_NULL_HANDLE) {
      vkDestroySwapchainKHR(device.get, swapChainHandle, null)
      swapChainHandle = VK_NULL_HANDLE
    }
    
    // These don't need explicit destruction as they're just wrapper objects
    swapChainImages = null
    swapChainExtent = null
  }
  
  /**
   * Close and clean up all resources
   */
  override protected def close(): Unit = {
    cleanup()
  }
}