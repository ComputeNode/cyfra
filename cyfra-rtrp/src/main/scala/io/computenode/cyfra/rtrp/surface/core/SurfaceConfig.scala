package io.computenode.cyfra.rtrp.surface.core

import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.KHRSwapchain.*
// Configuration for surface creation
case class SurfaceConfig(
  preferredFormat: Int = VK_FORMAT_B8G8R8A8_SRGB,
  preferredColorSpace: Int = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
  preferredPresentMode: Int = VK_PRESENT_MODE_MAILBOX_KHR,
  enableVSync: Boolean = true,
  minImageCount: Option[Int] = None,
  maxImageCount: Option[Int] = None,
):

// Create a copy with different format, present mode, VSync settings, or image count constraints
  def withFormat(format: Int): SurfaceConfig =
    copy(preferredFormat = format)

  def withPresentMode(mode: Int): SurfaceConfig =
    copy(preferredPresentMode = mode)

  def withVSync(enabled: Boolean): SurfaceConfig =
    val mode = if enabled then VK_PRESENT_MODE_FIFO_KHR else VK_PRESENT_MODE_IMMEDIATE_KHR
    copy(enableVSync = enabled, preferredPresentMode = mode)

  def withImageCount(min: Int, max: Int): SurfaceConfig =
    copy(minImageCount = Some(min), maxImageCount = Some(max))

// Predefined surface configurations.
object SurfaceConfig:

  def default: SurfaceConfig = SurfaceConfig()

  def gaming: SurfaceConfig = SurfaceConfig(
    preferredFormat = VK_FORMAT_B8G8R8A8_SRGB,
    preferredPresentMode = VK_PRESENT_MODE_MAILBOX_KHR,
    enableVSync = false,
    minImageCount = Some(2),
    maxImageCount = Some(3),
  )

  def quality: SurfaceConfig = SurfaceConfig(
    preferredFormat = VK_FORMAT_R8G8B8A8_SRGB,
    preferredColorSpace = 1000104001 ,
    preferredPresentMode = VK_PRESENT_MODE_FIFO_KHR,
    enableVSync = true,
  )

  def lowLatency: SurfaceConfig = SurfaceConfig(
    preferredFormat = VK_FORMAT_B8G8R8A8_UNORM,
    preferredPresentMode = VK_PRESENT_MODE_IMMEDIATE_KHR,
    enableVSync = false,
    minImageCount = Some(1),
    maxImageCount = Some(2),
  )
