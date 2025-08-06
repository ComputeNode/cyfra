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
import org.lwjgl.vulkan.{VkExtent2D, VkSwapchainCreateInfoKHR, VkImageViewCreateInfo, VkSurfaceFormatKHR, VkPresentInfoKHR, VkSemaphoreCreateInfo, VkSurfaceCapabilitiesKHR}
import scala.util.{Try, Success, Failure}

import scala.collection.mutable.ArrayBuffer

private[cyfra] class SwapchainManager (context: VulkanContext, surface: Surface) : Swapchain = (

    private val device = context.device
    private val physicalDevice = device.physicalDevice
    private var swapchainHandle: Long = VK_NULL_HANDLE
    private var swapchainImages: Array[Long] = _

    private var swapchainImageFormat: Int = _
    private var swapchainColorSpace: Int = _
    private var swapchainPresentMode: Int = _
    private var swapchainExtent: VkExtent2D = _
    private var swapchainImageViews: Array[Long] = _

    // Get the raw Vulkan capabilities for low-level access
    private val vkCapabilities = pushStack: stack =>
        val vkCapabilities = VkSurfaceCapabilitiesKHR.calloc(stack)
        check(
            vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice, surface.nativeHandle, vkCapabilities),
            "Failed to get surface capabilities"
        )
        vkCapabilities

    // Get the high-level surface capabilities for format/mode queries
    private val surfaceCapabilities = surface.getCapabilities() match
        case Success(caps) => caps
        case Failure(exception) =>
            throw new RuntimeException("Failed to get surface capabilities", exception)
    
    val (width, height) = (vkCapabilities.currentExtent().width(), vkCapabilities.currentExtent().height())
    val minImageExtent = vkCapabilities.minImageExtent()
    val maxImageExtent = vkCapabilities.maxImageExtent()

    def initialize (surfaceConfig: SurfaceConfig): Swapchain = pushStack: stack =>
        //cleanup()
        
        val preferredPresentMode = surfaceConfig.preferredPresentMode

        // Use the surface capabilities abstraction
        val availableFormats: List[Int] = surfaceCapabilities.supportedFormats
        val availableColorSpaces: List[Int] = surfaceCapabilities.supportedColorSpaces

        val exactFmtMatch = availableFormats.find(fmt => fmt == surfaceConfig.preferredFormat)
        val exactCsMatch = availableColorSpaces.find(cs => cs == surfaceConfig.preferredColorSpace)
        
        val chosenFormat = exactFmtMatch.orElse(availableFormats.headOption).getOrElse(
          throw new RuntimeException("No supported surface formats available")
        )
        val chosenColorSpace = exactCsMatch.orElse(availableColorSpaces.headOption).getOrElse(
          throw new RuntimeException("No supported color spaces available")
        )
        
        // Choose present mode
        val availableModes = surfaceCapabilities.supportedPresentModes
        val presentMode = if (availableModes.contains(preferredPresentMode)) preferredPresentMode else VK_PRESENT_MODE_FIFO_KHR

        // Choose swap extent
        val (chosenWidth, chosenHeight) = if (width != -1 && height != -1) 
            (width, height)
        else 
            val (desiredWidth, desiredHeight) = (800, 600) // TODO: get from window/config
            if surfaceCapabilities.isExtentSupported(desiredWidth, desiredHeight) then
                (desiredWidth, desiredHeight)
            else 
                surfaceCapabilities.clampExtent(desiredWidth, desiredHeight)
        
        // Determine image count
        var imageCount = surfaceCapabilities.minImageCount + 1
        if (surfaceCapabilities.maxImageCount != 0)
            imageCount = Math.min(imageCount, surfaceCapabilities.maxImageCount)

        // Convert from surface abstraction to Vulkan constants
        swapchainImageFormat = chosenFormat
        swapchainColorSpace = chosenColorSpace
        swapchainPresentMode = presentMode
        swapchainExtent = VkExtent2D.calloc(stack).width(chosenWidth).height(chosenHeight)

        // Create swapchain
        val createInfo = VkSwapchainCreateInfoKHR.calloc(stack)
          .sType$Default()
          .surface(surface.nativeHandle)
          .minImageCount(imageCount)
          .imageFormat(swapchainImageFormat)
          .imageColorSpace(swapchainColorSpace)
          .imageExtent(swapchainExtent)
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

        val pSwapchain = stack.callocLong(1)

        val result = vkCreateSwapchainKHR(device.get, createInfo, null, pSwapchain)
        if (result != VK_SUCCESS)
            throw new VulkanAssertionError("Failed to create swap chain", result)

        swapchainHandle = pSwapchain.get(0)

        // Get swap chain images
        val pImageCount = stack.callocInt(1)
        vkGetSwapchainImagesKHR(device.get, swapchainHandle, pImageCount, null)
        val actualImageCount = pImageCount.get(0)

        val pSwapchainImages = stack.callocLong(actualImageCount)
        vkGetSwapchainImagesKHR(device.get, swapchainHandle, pImageCount, pSwapchainImages)

        swapchainImages = new Array[Long](actualImageCount)
        for (i <- 0 until actualImageCount)
            swapchainImages(i) = pSwapchainImages.get(i)

        createImageViews() 

        Swapchain(
            handle = swapchainHandle
            images = swapchainImages
            imageViews = swapchainImageViews
            format = swapchainImageFormat,
            colorSpace = swapchainColorSpace,
            extent = swapchainExtent
        )

    private def createImageViews(): Unit = pushStack: Stack => 
        if (swapchainImages == null || swapchainImages.isEmpty)
            throw new VulkanAssertionError("Cannot create image views: swap chain images not initialized", -1)

        if (swapchainImageViews != null) 
            swapchainImageViews.foreach(imageView => 
                if (imageView != VK_NULL_HANDLE) 
                    vkDestroyImageView(device.get, imageView, null)
            )

        swapchainImageViews = new Array[Long](swapchainImages.length)

        for (i <- swapchainImages.indices)
            val createInfo = VkImageViewCreateInfo.calloc(stack)
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
            
            val pImageView = stack.callocLong(1)
            check (
                vkCreateImageView(device.get, createInfo, null, pImageView), 
                s"Failed to create image view for swap chain image $i"
            )
            swapchainImageViews(i) = pImageView.get(i)
)