package io.computenode.cyfra.rtrp.surface.core

// Configuration for surface creation
case class SurfaceConfig(
  preferredFormat: SurfaceFormat = SurfaceFormat.B8G8R8A8_SRGB,
  preferredColorSpace: ColorSpace = ColorSpace.SRGB_NONLINEAR,
  preferredPresentMode: PresentMode = PresentMode.MAILBOX,
  enableVSync: Boolean = true,
  minImageCount: Option[Int] = None,
  maxImageCount: Option[Int] = None,
):

// Create a copy with different format, present mode, VSync settings, or image count constraints
  def withFormat(format: SurfaceFormat): SurfaceConfig =
    copy(preferredFormat = format)

  def withPresentMode(mode: PresentMode): SurfaceConfig =
    copy(preferredPresentMode = mode)

  def withVSync(enabled: Boolean): SurfaceConfig =
    val mode = if enabled then PresentMode.FIFO else PresentMode.IMMEDIATE
    copy(enableVSync = enabled, preferredPresentMode = mode)

  def withImageCount(min: Int, max: Int): SurfaceConfig =
    copy(minImageCount = Some(min), maxImageCount = Some(max))

// Predefined surface configurations.
object SurfaceConfig:

  def default: SurfaceConfig = SurfaceConfig()

  def gaming: SurfaceConfig = SurfaceConfig(
    preferredFormat = SurfaceFormat.B8G8R8A8_SRGB,
    preferredPresentMode = PresentMode.MAILBOX,
    enableVSync = false,
    minImageCount = Some(2),
    maxImageCount = Some(3),
  )

  def quality: SurfaceConfig = SurfaceConfig(
    preferredFormat = SurfaceFormat.R8G8B8A8_SRGB,
    preferredColorSpace = ColorSpace.DISPLAY_P3_NONLINEAR,
    preferredPresentMode = PresentMode.FIFO,
    enableVSync = true,
  )

  def lowLatency: SurfaceConfig = SurfaceConfig(
    preferredFormat = SurfaceFormat.B8G8R8A8_UNORM,
    preferredPresentMode = PresentMode.IMMEDIATE,
    enableVSync = false,
    minImageCount = Some(1),
    maxImageCount = Some(2),
  )
