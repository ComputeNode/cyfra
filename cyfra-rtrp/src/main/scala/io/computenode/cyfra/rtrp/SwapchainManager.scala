package io.computenode.cyfra.rtrp

import io.computenode.cyfra.vulkan.VulkanContext
import io.computenode.cyfra.vulkan.util.Util.{check, pushStack}
import io.computenode.cyfra.rtrp.surface.core.*
import io.computenode.cyfra.rtrp.surface.vulkan.*
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.KHRSwapchain.*
import org.lwjgl.vulkan.VK10.*
import io.computenode.cyfra.vulkan.util.{VulkanAssertionError, VulkanObjectHandle}
import org.lwjgl.vulkan.{
  VkExtent2D,
  VkSwapchainCreateInfoKHR,
  VkImageViewCreateInfo,
  VkSurfaceFormatKHR,
  VkPresentInfoKHR,
  VkSemaphoreCreateInfo,
  VkSurfaceCapabilitiesKHR,
  VkFramebufferCreateInfo,
}
import scala.util.{Try, Success, Failure}

import scala.collection.mutable.ArrayBuffer

private[cyfra] class SwapchainManager(context: VulkanContext, surface: Surface):

  private val device = context.device
  private val physicalDevice = device.physicalDevice
  private var swapchainHandle: Long = VK_NULL_HANDLE
  private var swapchainImages: Array[Long] = _

  private var swapchainImageFormat: Int = _
  private var swapchainColorSpace: Int = _
  private var swapchainPresentMode: Int = _
  private var swapchainWidth: Int = 0
  private var swapchainHeight: Int = 0
  private var swapchainImageViews: Array[Long] = _

  def cleanup(): Unit =
    if swapchainImageViews != null then
      swapchainImageViews.foreach(iv => if iv != VK_NULL_HANDLE then vkDestroyImageView(device.get, iv, null))
      swapchainImageViews = null

    if swapchainHandle != VK_NULL_HANDLE then
      vkDestroySwapchainKHR(device.get, swapchainHandle, null)
      swapchainHandle = VK_NULL_HANDLE

  // Get the high-level surface capabilities for format/mode queries
  private val surfaceCapabilities = surface.getCapabilities() match
    case Success(caps)      => caps
    case Failure(exception) =>
      throw new RuntimeException("Failed to get surface capabilities", exception)

  def initialize(surfaceConfig: SurfaceConfig): Swapchain = pushStack: Stack =>
    cleanup()

    val vkCapabilities = VkSurfaceCapabilitiesKHR.calloc(Stack)
    check(vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice, surface.nativeHandle, vkCapabilities), "Failed to get surface capabilities")

    val (width, height) = (vkCapabilities.currentExtent().width(), vkCapabilities.currentExtent().height())
    val minImageExtent = vkCapabilities.minImageExtent()
    val maxImageExtent = vkCapabilities.maxImageExtent()

    val preferredPresentMode = surfaceConfig.preferredPresentMode

    val availableSurfaceFormats: List[VkSurfaceFormatKHR] = surfaceCapabilities.vkSurfaceFormats
    val preferredFormat = surfaceConfig.preferredFormat
    val preferredColorSpace = surfaceConfig.preferredColorSpace

    val chosenSurfaceFormat = availableSurfaceFormats
      .find(f => f.format() == preferredFormat && f.colorSpace() == preferredColorSpace)
      .orElse(availableSurfaceFormats.headOption)
      .getOrElse(throw new RuntimeException("No supported surface formats available"))

    val chosenFormat = chosenSurfaceFormat.format()
    val chosenColorSpace = chosenSurfaceFormat.colorSpace()

    // Choose present mode
    val availableModes = surfaceCapabilities.supportedPresentModes
    val presentMode = if availableModes.contains(preferredPresentMode) then preferredPresentMode else VK_PRESENT_MODE_FIFO_KHR

    // Choose swap extent
    val (chosenWidth, chosenHeight) =
      if width > 0 && height > 0 then (width, height)
      else
        val (desiredWidth, desiredHeight) = (800, 600)
        if surfaceCapabilities.isExtentSupported(desiredWidth, desiredHeight) then (desiredWidth, desiredHeight)
        else surfaceCapabilities.clampExtent(desiredWidth, desiredHeight)

    // Determine image count
    var imageCount = surfaceCapabilities.minImageCount + 1
    if surfaceCapabilities.maxImageCount != 0 then imageCount = Math.min(imageCount, surfaceCapabilities.maxImageCount)

    // Convert from surface abstraction to Vulkan constants
    swapchainImageFormat = chosenFormat
    swapchainColorSpace = chosenColorSpace
    swapchainPresentMode = presentMode
    swapchainWidth = chosenWidth
    swapchainHeight = chosenHeight
    // Create swapchain
    val createInfo = VkSwapchainCreateInfoKHR
      .calloc(Stack)
      .sType$Default()
      .surface(surface.nativeHandle)
      .minImageCount(imageCount)
      .imageFormat(swapchainImageFormat)
      .imageColorSpace(swapchainColorSpace)
      .imageExtent(VkExtent2D.calloc(Stack).width(swapchainWidth).height(swapchainHeight))
      .imageArrayLayers(1)
      .imageUsage(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
      .preTransform(vkCapabilities.currentTransform())
      .compositeAlpha(VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
      .presentMode(swapchainPresentMode)
      .clipped(true)
      .oldSwapchain(VK_NULL_HANDLE)
      .imageSharingMode(VK_SHARING_MODE_EXCLUSIVE)
      .queueFamilyIndexCount(0)
      .pQueueFamilyIndices(null)

    val pSwapchain = Stack.callocLong(1)

    val result = vkCreateSwapchainKHR(device.get, createInfo, null, pSwapchain)
    if result != VK_SUCCESS then throw new VulkanAssertionError("Failed to create swap chain", result)

    swapchainHandle = pSwapchain.get(0)

    // Get swap chain images
    val pImageCount = Stack.callocInt(1)
    vkGetSwapchainImagesKHR(device.get, swapchainHandle, pImageCount, null)
    val actualImageCount = pImageCount.get(0)

    val pSwapchainImages = Stack.callocLong(actualImageCount)
    vkGetSwapchainImagesKHR(device.get, swapchainHandle, pImageCount, pSwapchainImages)

    swapchainImages = new Array[Long](actualImageCount)
    for i <- 0 until actualImageCount do swapchainImages(i) = pSwapchainImages.get(i)

    createImageViews()

    Swapchain(
      device = device.get,
      handle = swapchainHandle,
      images = swapchainImages,
      imageViews = swapchainImageViews,
      format = swapchainImageFormat,
      colorSpace = swapchainColorSpace,
      width = swapchainWidth,
      height = swapchainHeight,
    )

  private def createImageViews(): Unit = pushStack: Stack =>
    if swapchainImages == null || swapchainImages.isEmpty then
      throw new VulkanAssertionError("Cannot create image views: swap chain images not initialized", -1)

    if swapchainImageViews != null then
      swapchainImageViews.foreach(imageView => if imageView != VK_NULL_HANDLE then vkDestroyImageView(device.get, imageView, null))

    swapchainImageViews = new Array[Long](swapchainImages.length)

    try
      for i <- swapchainImages.indices do
        val createInfo = VkImageViewCreateInfo
          .calloc(Stack)
          .sType$Default()
          .image(swapchainImages(i))
          .viewType(VK_IMAGE_VIEW_TYPE_2D)
          .format(swapchainImageFormat)

        createInfo.components: components =>
          components
            .r(VK_COMPONENT_SWIZZLE_IDENTITY)
            .g(VK_COMPONENT_SWIZZLE_IDENTITY)
            .b(VK_COMPONENT_SWIZZLE_IDENTITY)
            .a(VK_COMPONENT_SWIZZLE_IDENTITY)

        createInfo.subresourceRange: range =>
          range
            .aspectMask(VK_IMAGE_ASPECT_COLOR_BIT)
            .baseMipLevel(0)
            .levelCount(1)
            .baseArrayLayer(0)
            .layerCount(1)

        val pImageView = Stack.callocLong(1)
        check(vkCreateImageView(device.get, createInfo, null, pImageView), s"Failed to create image view for swap chain image $i")
        swapchainImageViews(i) = pImageView.get(0)
    catch
      case ex: Throwable =>
        if swapchainImageViews != null then
          swapchainImageViews.foreach: iv =>
            if iv != 0L && iv != VK_NULL_HANDLE then
              try vkDestroyImageView(device.get, iv, null)
              catch case _: Throwable => ()
          swapchainImageViews = null
        throw ex

  def destroyImageViews(swapchain: Swapchain): Unit =
    if swapchain.imageViews != null then
      swapchain.imageViews.foreach: iv =>
        if iv != VK_NULL_HANDLE then vkDestroyImageView(device.get, iv, null)

  def destroySwapchain(swapchain: Swapchain): Unit =
    if swapchain.handle != VK_NULL_HANDLE then vkDestroySwapchainKHR(device.get, swapchain.handle, null)

object SwapchainManager:
  def createFramebuffers(swapchain: Swapchain, renderPass: Long): Array[Long] = pushStack: Stack =>
    val swapchainFramebuffers = new Array[Long](swapchain.imageViews.length)
    for i <- swapchain.imageViews.indices do
      val attachments = Stack.callocLong(1)
      attachments.put(0, swapchain.imageViews(i))

      val framebufferInfo = VkFramebufferCreateInfo
        .calloc(Stack)
        .sType$Default()
        .renderPass(renderPass)
        .pAttachments(attachments)
        .width(swapchain.width)
        .height(swapchain.height)
        .layers(1)

      val pFrameBuffer = Stack.callocLong(1)
      check(vkCreateFramebuffer(swapchain.device, framebufferInfo, null, pFrameBuffer), s"Failed to create framebuffer $i")
      swapchainFramebuffers(i) = pFrameBuffer.get(0)

    swapchainFramebuffers
