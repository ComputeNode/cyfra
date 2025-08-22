package io.computenode.cyfra.rtrp.surface.vulkan

import io.computenode.cyfra.rtrp.surface.core.*
import io.computenode.cyfra.vulkan.VulkanContext
import org.lwjgl.system.MemoryStack
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.*
import scala.jdk.CollectionConverters.*
import scala.util.*

class VulkanSurfaceCapabilities(
    vulkanContext: VulkanContext,
    surface: VulkanSurface
) extends SurfaceCapabilities:

  // Query and copy primitive capability values into safe fields
  private val (
    minImageExtentTuple,
    maxImageExtentTuple,
    currentExtentTuple,
    minImageCountVal,
    maxImageCountVal,
    supportsAlphaVal,
    supportsTransformVal
  ) = {
    MemoryStack.stackPush()
    try
      val stack = MemoryStack.stackGet()
      val caps = VkSurfaceCapabilitiesKHR.callocStack(stack)
      val result = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(
        vulkanContext.device.physicalDevice,
        surface.nativeHandle,
        caps
      )
      if result != VK_SUCCESS then
        throw new RuntimeException(s"Failed to get surface capabilities: $result")

      val minExtent = (caps.minImageExtent().width(), caps.minImageExtent().height())
      val maxExtent = (caps.maxImageExtent().width(), caps.maxImageExtent().height())
      val curExtent = caps.currentExtent()
      val current =
        if curExtent.width() == 0xffffffff || curExtent.height() == 0xffffffff then
          (-1, -1)
        else
          (curExtent.width(), curExtent.height())

      val supportsAlpha =
        (caps.supportedCompositeAlpha() & VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR) != 0
      val supportsTransform =
        (caps.supportedTransforms() & VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR) != 0

      val minImageCount = caps.minImageCount()
      val maxImageCount =
        if caps.maxImageCount() == 0 then Int.MaxValue else caps.maxImageCount()

      (
        minExtent,
        maxExtent,
        current,
        minImageCount,
        maxImageCount,
        supportsAlpha,
        supportsTransform
      )
    finally MemoryStack.stackPop()
  }

  // Query formats and present modes once and copy into safe lists of ints
  private val (
    formatsList,
    colorSpacesList,
    presentModesList
  ) = {
    MemoryStack.stackPush()
    try
      val stack = MemoryStack.stackGet()

      // Surface formats
      val pFormatCount = stack.callocInt(1)
      vkGetPhysicalDeviceSurfaceFormatsKHR(
        vulkanContext.device.physicalDevice,
        surface.nativeHandle,
        pFormatCount,
        null
      )
      val formatCount = pFormatCount.get(0)
      val formats =
        if formatCount == 0 then List.empty[(Int, Int)]
        else
          val fmtBuf = VkSurfaceFormatKHR.callocStack(formatCount, stack)
          vkGetPhysicalDeviceSurfaceFormatsKHR(
            vulkanContext.device.physicalDevice,
            surface.nativeHandle,
            pFormatCount,
            fmtBuf
          )
          (0 until formatCount).map { i =>
            (fmtBuf.get(i).format(), fmtBuf.get(i).colorSpace())
          }.toList

      val (formatOnly, colorSpaceOnly) = formats.unzip

      // Present modes
      val pModeCount = stack.callocInt(1)
      vkGetPhysicalDeviceSurfacePresentModesKHR(
        vulkanContext.device.physicalDevice,
        surface.nativeHandle,
        pModeCount,
        null
      )
      val modeCount = pModeCount.get(0)
      val presentModes =
        if modeCount == 0 then List.empty[Int]
        else
          val modesBuf = stack.callocInt(modeCount)
          vkGetPhysicalDeviceSurfacePresentModesKHR(
            vulkanContext.device.physicalDevice,
            surface.nativeHandle,
            pModeCount,
            modesBuf
          )
          (0 until modeCount).map(modesBuf.get).toList

      (formatOnly, colorSpaceOnly, presentModes)
    finally MemoryStack.stackPop()
  }

  // SurfaceCapabilities trait implementations (safe, heap-backed)
  override def supportedFormats: List[Int] = formatsList
  override def supportedColorSpaces: List[Int] = colorSpacesList
  override def supportedPresentModes: List[Int] = presentModesList

  override def minImageExtent: (Int, Int) = minImageExtentTuple
  override def maxImageExtent: (Int, Int) = maxImageExtentTuple
  override def currentExtent: (Int, Int) = currentExtentTuple

  override def minImageCount: Int = minImageCountVal
  override def maxImageCount: Int = maxImageCountVal
  override def supportsAlpha: Boolean = supportsAlphaVal
  override def supportsTransform: Boolean = supportsTransformVal

  override def vkSurfaceFormats: List[VkSurfaceFormatKHR] =
    val stack = MemoryStack.stackGet()
    if stack == null then
      throw new RuntimeException("vkSurfaceFormats must be called with an active MemoryStack")
    val pFormatCount = stack.callocInt(1)
    vkGetPhysicalDeviceSurfaceFormatsKHR(
      vulkanContext.device.physicalDevice,
      surface.nativeHandle,
      pFormatCount,
      null
    )
    val fmtCount = pFormatCount.get(0)
    if fmtCount == 0 then List.empty
    else
      val fmtBuf = VkSurfaceFormatKHR.calloc(fmtCount, stack)
      vkGetPhysicalDeviceSurfaceFormatsKHR(
        vulkanContext.device.physicalDevice,
        surface.nativeHandle,
        pFormatCount,
        fmtBuf
      )
      (0 until fmtCount).map(fmtBuf.get).toList

  // Helpers used by higher-level code
  override def isExtentSupported(w: Int, h: Int): Boolean =
    val (minW, minH) = minImageExtent
    val (maxW, maxH) = maxImageExtent
    w >= minW && h >= minH && w <= maxW && h <= maxH

  override def clampExtent(w: Int, h: Int): (Int, Int) =
    val (minW, minH) = minImageExtent
    val (maxW, maxH) = maxImageExtent
    (
      Math.max(minW, Math.min(w, maxW)),
      Math.max(minH, Math.min(h, maxH))
    )
