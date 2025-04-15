package io.computenode.cyfra.vulkan.core

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack} // Ensure pushStack is imported
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryUtil.memAddress // Import memAddress
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.KHRSwapchain.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.{VkExtent2D, VkSwapchainCreateInfoKHR, VkImageViewCreateInfo, VkSurfaceFormatKHR, VkPresentInfoKHR, VkSemaphoreCreateInfo, VkSurfaceCapabilitiesKHR} // Added VkSurfaceCapabilitiesKHR

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
  
  // Synchronization objects for rendering
  private var imageAvailableSemaphores: Array[Long] = _
  private var renderFinishedSemaphores: Array[Long] = _
  private var inFlightFences: Array[Long] = _
  private var imagesInFlight: Array[Long] = _
  private var currentFrame = 0
  
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

  // Removed chooseSwapSurfaceFormat, chooseSwapPresentMode, chooseSwapExtent, determineImageCount methods
  // Their logic is now inlined within initialize

  /**
   * Initialize the swap chain with specified dimensions
   * 
   * @param width The current window width
   * @param height The current window height
   * @return True if initialization was successful
   */
  def initialize(width: Int, height: Int): Boolean = pushStack { stack =>
    cleanup()

    // --- Query Surface Capabilities ONCE ---
    val capabilities = VkSurfaceCapabilitiesKHR.calloc(stack)
    check(
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice, surface.get, capabilities),
      "Failed to get surface capabilities"
    )
    val currentTransform = capabilities.currentTransform()
    val minImageCountCap = capabilities.minImageCount()
    val maxImageCountCap = capabilities.maxImageCount()
    val currentExtentCap = capabilities.currentExtent()
    val minExtentCap = capabilities.minImageExtent()
    val maxExtentCap = capabilities.maxImageExtent()

    // --- Choose Surface Format ---
    val availableFormats: List[(Int, Int)] = context.getSurfaceFormats(surface)
    val preferredFormat = VK_FORMAT_B8G8R8A8_SRGB
    val preferredColorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
    println(s"--- Surface Format Selection ---") // Keep debug prints for now
    println(s"Available Formats (${availableFormats.length}):")
    availableFormats.foreach(f => println(s"  Format: ${f._1}, ColorSpace: ${f._2}"))
    println(s"Preferred Format: $preferredFormat, Preferred ColorSpace: $preferredColorSpace")
    if (availableFormats.isEmpty) {
      throw new VulkanAssertionError("No surface formats available", -1)
    }
    val exactMatch = availableFormats.find { case (fmt, cs) => fmt == preferredFormat && cs == preferredColorSpace }
    val formatMatch = availableFormats.find { case (fmt, _) => fmt == preferredFormat }
    val (chosenFormat, chosenColorSpace) = exactMatch.orElse(formatMatch).getOrElse(availableFormats.head)
    println(s"Chosen Format: $chosenFormat, Chosen ColorSpace: $chosenColorSpace")
    println(s"--- End Surface Format Selection ---")

    // --- Choose Present Mode ---
    val availableModes = context.getPresentModes(surface)
    val preferredMode = VK_PRESENT_MODE_MAILBOX_KHR
    val presentMode = if (availableModes.contains(preferredMode)) preferredMode else VK_PRESENT_MODE_FIFO_KHR

    // --- Choose Swap Extent ---
    println(s"--- Swap Extent Selection ---") // Keep debug prints
    println(s"Requested Extent: ${width}x${height}")
    println(s"Capabilities Current Extent: ${currentExtentCap.width()}x${currentExtentCap.height()}")
    println(s"Capabilities Min Extent: ${minExtentCap.width()}x${minExtentCap.height()}")
    println(s"Capabilities Max Extent: ${maxExtentCap.width()}x${maxExtentCap.height()}")
    val (chosenWidth, chosenHeight) = if (currentExtentCap.width() != -1) { // Use -1 for UINT32_MAX check
      (currentExtentCap.width(), currentExtentCap.height())
    } else {
      val w = Math.max(minExtentCap.width(), Math.min(maxExtentCap.width(), width))
      val h = Math.max(minExtentCap.height(), Math.min(maxExtentCap.height(), height))
      (w, h)
    }
    println(s"Final Chosen Extent: ${chosenWidth}x${chosenHeight}")
    println(s"--- End Swap Extent Selection ---")

    // --- Determine Image Count ---
    var imageCount = minImageCountCap + 1
    if (maxImageCountCap > 0) {
      imageCount = Math.min(imageCount, maxImageCountCap)
    }
    // Optional: Ensure enough images for frames in flight
    // imageCount = Math.max(imageCount, MAX_FRAMES_IN_FLIGHT)
    // if (maxImageCountCap > 0) {
    //   imageCount = Math.min(imageCount, maxImageCountCap)
    // }
    swapChainImageCount = imageCount // Store the final count

    swapChainImageFormat = chosenFormat
    swapChainColorSpace = chosenColorSpace
    swapChainPresentMode = presentMode

    // --- Debugging: Check Queue Family Presentation Support ---
    val computeQueueFamily = context.computeQueue.familyIndex
    val supportsPresent = context.isQueueFamilyPresentSupported(computeQueueFamily, surface)
    println(s"--- Queue Family Presentation Check ---")
    println(s"Compute Queue Family Index: $computeQueueFamily")
    println(s"Supports presentation to surface: $supportsPresent")
    if (!supportsPresent) {
        // This might be the root cause if presentation is needed from this queue
        println("ERROR: The selected compute queue family does not support presentation to this surface!")
        // Optionally throw an error to halt execution if this is critical
        // throw new VulkanAssertionError(s"Queue family $computeQueueFamily does not support presentation to the surface", -1)
    }
    println(s"--- End Queue Family Presentation Check ---")
    // --- End Debugging ---

    // Store selected format, extent, and present mode
    swapChainExtent = VkExtent2D.calloc(stack).width(chosenWidth).height(chosenHeight)
    swapChainImageFormat = chosenFormat
    swapChainColorSpace = chosenColorSpace
    swapChainPresentMode = presentMode

    // Create the swap chain
    val createInfo = VkSwapchainCreateInfoKHR.calloc(stack)
      .sType$Default()
      .surface(surface.get)
      .minImageCount(imageCount)
      .imageFormat(swapChainImageFormat)
      .imageColorSpace(swapChainColorSpace)
      .imageExtent(swapChainExtent)
      .imageArrayLayers(1)
      .imageUsage(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT) // Added TRANSFER_DST
      .preTransform(currentTransform) // Use transform queried earlier
      .compositeAlpha(VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
      .presentMode(swapChainPresentMode)
      .clipped(true)
      .oldSwapchain(VK_NULL_HANDLE)
      .imageSharingMode(VK_SHARING_MODE_EXCLUSIVE)
      .queueFamilyIndexCount(0)
      .pQueueFamilyIndices(null)

    // --- Debugging Start ---
    println(s"--- SwapChain Creation Info ---") // Keep debug prints
    println(s"Device Handle: ${device.get}")
    println(s"Surface Handle: ${surface.get}")
    println(s"Min Image Count: ${createInfo.minImageCount()}")
    println(s"Image Format: ${createInfo.imageFormat()}")
    println(s"Image Color Space: ${createInfo.imageColorSpace()}")
    println(s"Image Extent: ${createInfo.imageExtent().width()}x${createInfo.imageExtent().height()}")
    println(s"Image Usage: ${createInfo.imageUsage()}")
    println(s"Pre Transform: ${createInfo.preTransform()}")
    println(s"Present Mode: ${createInfo.presentMode()}")
    println(s"Sharing Mode: ${createInfo.imageSharingMode()}")
    println(s"--- End SwapChain Creation Info ---")
    // --- Debugging End ---

    // --- Explicit Handle Checks ---
    // Use .address() to get the underlying Long handle for VkDevice
    if (device.get.address() == VK_NULL_HANDLE) { 
        throw new VulkanAssertionError("Device handle is VK_NULL_HANDLE before vkCreateSwapchainKHR", -1)
    }
    if (surface.get == VK_NULL_HANDLE) {
        throw new VulkanAssertionError("Surface handle is VK_NULL_HANDLE before vkCreateSwapchainKHR", -1)
    }
    if (createInfo == null) {
        throw new VulkanAssertionError("VkSwapchainCreateInfoKHR struct is null before vkCreateSwapchainKHR", -1)
    }
    // --- End Explicit Handle Checks ---

    // Create the swap chain
    val pSwapChain = stack.callocLong(1)
    // --- Debugging: Log addresses ---
    // Use memAddress for NIO buffers
    println(s"Calling vkCreateSwapchainKHR with Device: ${device.get}, CreateInfo Address: ${createInfo.address()}, pSwapChain Address: ${memAddress(pSwapChain)}") 
    // --- End Debugging ---

    val result = vkCreateSwapchainKHR(device.get, createInfo, null, pSwapChain)
    if (result != VK_SUCCESS) {
      // --- Debugging: Print result code on failure ---
      println(s"vkCreateSwapchainKHR failed with result code: $result (${VulkanAssertionError.translateVulkanResult(result)})")
      // --- End Debugging ---
      throw new VulkanAssertionError("Failed to create swap chain", result)
    }
    swapChainHandle = pSwapChain.get(0)
    // --- Debugging: Log success ---
    println(s"vkCreateSwapchainKHR succeeded. SwapChain Handle: $swapChainHandle")
    // --- End Debugging ---

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
    
    // Create synchronization objects
    createSynchronizationObjects()
    
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
          .baseMipLevel(0)
          .levelCount(1)
          .baseArrayLayer(0)
          .layerCount(1)
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
   * Create synchronization objects for rendering
   */
  private def createSynchronizationObjects(): Unit = pushStack { stack =>
    // Create the semaphores and fences for frame synchronization
    imageAvailableSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT)
    renderFinishedSemaphores = new Array[Long](MAX_FRAMES_IN_FLIGHT)
    inFlightFences = new Array[Long](MAX_FRAMES_IN_FLIGHT)
    imagesInFlight = new Array[Long](swapChainImages.length)
    
    // Initialize imagesInFlight array with VK_NULL_HANDLE
    for (i <- imagesInFlight.indices) {
      imagesInFlight(i) = VK_NULL_HANDLE
    }
    
    // Semaphore creation info
    val semaphoreInfo = VkSemaphoreCreateInfo.calloc(stack)
      .sType$Default()
    
    // Fence creation info - create in signaled state so first frame doesn't wait
    val fenceInfo = org.lwjgl.vulkan.VkFenceCreateInfo.calloc(stack)
      .sType$Default()
      .flags(VK_FENCE_CREATE_SIGNALED_BIT)
    
    for (i <- 0 until MAX_FRAMES_IN_FLIGHT) {
      // Create imageAvailable semaphore
      val pImageAvailableSemaphore = stack.callocLong(1)
      check(
        vkCreateSemaphore(device.get, semaphoreInfo, null, pImageAvailableSemaphore),
        s"Failed to create image available semaphore for frame $i"
      )
      imageAvailableSemaphores(i) = pImageAvailableSemaphore.get(0)
      
      // Create renderFinished semaphore
      val pRenderFinishedSemaphore = stack.callocLong(1)
      check(
        vkCreateSemaphore(device.get, semaphoreInfo, null, pRenderFinishedSemaphore),
        s"Failed to create render finished semaphore for frame $i"
      )
      renderFinishedSemaphores(i) = pRenderFinishedSemaphore.get(0)
      
      // Create inFlight fence
      val pFence = stack.callocLong(1)
      check(
        vkCreateFence(device.get, fenceInfo, null, pFence),
        s"Failed to create in-flight fence for frame $i"
      )
      inFlightFences(i) = pFence.get(0)
    }
  }
  
  /**
   * Creates command buffers for rendering to the swapchain images
   * 
   * @param commandPool The command pool to allocate command buffers from
   * @return Array of command buffers, one for each swap chain image
   */
  def createCommandBuffers(commandPool: Long): Array[org.lwjgl.vulkan.VkCommandBuffer] = pushStack { stack =>
    // Allocate one command buffer per swap chain image
    val allocInfo = org.lwjgl.vulkan.VkCommandBufferAllocateInfo.calloc(stack)
      .sType$Default()
      .commandPool(commandPool)
      .level(VK_COMMAND_BUFFER_LEVEL_PRIMARY)
      .commandBufferCount(swapChainImages.length)
      
    val pCommandBuffers = stack.mallocPointer(swapChainImages.length)
    check(
      vkAllocateCommandBuffers(device.get, allocInfo, pCommandBuffers),
      "Failed to allocate command buffers"
    )
    
    val commandBuffers = new Array[org.lwjgl.vulkan.VkCommandBuffer](swapChainImages.length)
    for (i <- 0 until swapChainImages.length) {
      commandBuffers(i) = new org.lwjgl.vulkan.VkCommandBuffer(pCommandBuffers.get(i), device.get)
    }
    
    commandBuffers
  }
  
  /**
   * Acquire the next image in the swap chain
   * 
   * @return The index of the acquired swap chain image
   */
  def acquireNextImage(): Int = pushStack { stack =>
    // Wait for the previous frame to finish
    check(
      vkWaitForFences(device.get, stack.longs(inFlightFences(currentFrame)), true, Long.MaxValue),
      "Failed to wait for fence"
    )
    
    // Get the next image
    val pImageIndex = stack.callocInt(1)
    val result = vkAcquireNextImageKHR(
      device.get,
      swapChainHandle,
      Long.MaxValue,
      imageAvailableSemaphores(currentFrame),
      VK_NULL_HANDLE,
      pImageIndex
    )
    
    if (result == VK_ERROR_OUT_OF_DATE_KHR) {
      // Swap chain is outdated, needs recreation
      return -1
    } else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR) {
      throw new VulkanAssertionError("Failed to acquire swap chain image", result)
    }
    
    val imageIndex = pImageIndex.get(0)
    
    // Check if a previous frame is using this image
    if (imagesInFlight(imageIndex) != VK_NULL_HANDLE) {
      check(
        vkWaitForFences(device.get, stack.longs(imagesInFlight(imageIndex)), true, Long.MaxValue),
        "Failed to wait for image fence"
      )
    }
    
    // Mark the image as being in use by this frame
    imagesInFlight(imageIndex) = inFlightFences(currentFrame)
    
    // Reset the fence for this frame
    check(
      vkResetFences(device.get, stack.longs(inFlightFences(currentFrame))),
      "Failed to reset fence"
    )
    
    imageIndex
  }
  
  /**
   * Submit a command buffer for rendering and present the image
   * 
   * @param commandBuffer The command buffer to submit
   * @param imageIndex The index of the swap chain image to present
   * @param queue The queue to submit the command buffer to
   * @return True if the presentation was successful
   */
  def submitAndPresent(commandBuffer: org.lwjgl.vulkan.VkCommandBuffer, imageIndex: Int, queue: org.lwjgl.vulkan.VkQueue): Boolean = pushStack { stack =>
    // Set up submit info
    val waitSemaphores = stack.longs(imageAvailableSemaphores(currentFrame))
    val waitStages = stack.ints(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
    val signalSemaphores = stack.longs(renderFinishedSemaphores(currentFrame))
    
    val submitInfo = org.lwjgl.vulkan.VkSubmitInfo.calloc(stack)
      .sType$Default()
      .pWaitSemaphores(waitSemaphores)
      .pWaitDstStageMask(waitStages)
      .pCommandBuffers(stack.pointers(commandBuffer))
      .pSignalSemaphores(signalSemaphores)
    
    // Submit the command buffer
    check(
      vkQueueSubmit(queue, submitInfo, inFlightFences(currentFrame)),
      "Failed to submit draw command buffer"
    )
    
    // Present the image
    val presentInfo = VkPresentInfoKHR.calloc(stack)
      .sType$Default()
      .pWaitSemaphores(signalSemaphores)
      .swapchainCount(1)
      .pSwapchains(stack.longs(swapChainHandle))
      .pImageIndices(stack.ints(imageIndex))
    
    val result = vkQueuePresentKHR(queue, presentInfo)
    
    // Update frame index for next frame
    currentFrame = (currentFrame + 1) % MAX_FRAMES_IN_FLIGHT
    
    if (result == VK_ERROR_OUT_OF_DATE_KHR || result == VK_SUBOPTIMAL_KHR) {
      // Swap chain needs recreation
      return false
    } else if (result != VK_SUCCESS) {
      throw new VulkanAssertionError("Failed to present swap chain image", result)
    }
    
    true
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
  
  // Added getter for single image
  def getImage(index: Int): Long = {
    if (swapChainImages == null || index < 0 || index >= swapChainImages.length) {
      throw new IndexOutOfBoundsException(s"Image index $index out of bounds (0 to ${if (swapChainImages == null) "null" else swapChainImages.length - 1})")
    }
    swapChainImages(index)
  }
  
  /**
   * Get the swap chain image views
   * 
   * @return Array of image view handles
   */
  def getImageViews: Array[Long] = swapChainImageViews
  
  /**
   * Get the current frame index
   * 
   * @return The current frame index
   */
  def getCurrentFrame: Int = currentFrame
  
  /**
   * Get the image available semaphore for the current frame
   * 
   * @return The image available semaphore handle
   */
  def getImageAvailableSemaphore: Long = imageAvailableSemaphores(currentFrame)
  
  /**
   * Get the render finished semaphore for the current frame
   * 
   * @return The render finished semaphore handle
   */
  def getRenderFinishedSemaphore: Long = renderFinishedSemaphores(currentFrame)
  
  /**
   * Get the in-flight fence for the current frame
   * 
   * @return The in-flight fence handle
   */
  def getInFlightFence: Long = inFlightFences(currentFrame)
  
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
    
    if (imageAvailableSemaphores != null) {
      imageAvailableSemaphores.foreach(semaphore => 
        if (semaphore != VK_NULL_HANDLE) {
          vkDestroySemaphore(device.get, semaphore, null)
        }
      )
      imageAvailableSemaphores = null
    }
    
    if (renderFinishedSemaphores != null) {
      renderFinishedSemaphores.foreach(semaphore => 
        if (semaphore != VK_NULL_HANDLE) {
          vkDestroySemaphore(device.get, semaphore, null)
        }
      )
      renderFinishedSemaphores = null
    }
    
    if (inFlightFences != null) {
      inFlightFences.foreach(fence => 
        if (fence != VK_NULL_HANDLE) {
          vkDestroyFence(device.get, fence, null)
        }
      )
      inFlightFences = null
    }
  }
  
  /**
   * Clean up all resources used by the swap chain manager
   */
  override def close(): Unit = {
    cleanup()
  }
}