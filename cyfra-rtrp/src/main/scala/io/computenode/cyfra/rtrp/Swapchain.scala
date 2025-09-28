package io.computenode.cyfra.rtrp

import org.lwjgl.vulkan.VkExtent2D
import org.lwjgl.vulkan.{VkDevice, VkExtent2D}
import org.lwjgl.vulkan.KHRSwapchain.vkDestroySwapchainKHR
import org.lwjgl.vulkan.VK10.vkDestroyImageView
import io.computenode.cyfra.vulkan.util.VulkanObjectHandle

private[cyfra] class Swapchain(
  val device: VkDevice,
  override val handle: Long,
  val images: Array[Long],
  val imageViews: Array[Long],
  val format: Int,
  val colorSpace: Int,
  val width: Int,
  val height: Int,
) extends VulkanObjectHandle:

  override def close(): Unit =
    if imageViews != null then
      imageViews.foreach: imageView =>
        if imageView != 0L then vkDestroyImageView(device, imageView, null)

    vkDestroySwapchainKHR(device, handle, null)
    alive = false
