package io.computenode.cyfra.rtrp.surface.core

// Surface capabilities - what the surface can do
trait SurfaceCapabilities:
  def supportedFormats: List[Int]
  def supportedColorSpaces: List[Int]
  def supportedPresentModes: List[Int]
  def minImageExtent: (Int, Int)
  def maxImageExtent: (Int, Int)
  def currentExtent: (Int, Int)
  def minImageCount: Int
  def maxImageCount: Int
  def supportsAlpha: Boolean
  def supportsTransform: Boolean

  def supportsFormat(format: Int): Boolean =
    supportedFormats.contains(format)

  def supportsPresentMode(mode: Int): Boolean =
    supportedPresentModes.contains(mode)

  def chooseBestFormat(preferences: List[Int]): Option[Int] =
    preferences.find(supportsFormat)

  def chooseBestPresentMode(preferences: List[Int]): Option[Int] =
    preferences.find(supportsPresentMode)

  // Check if the given extent is within supported bounds
  def isExtentSupported(width: Int, height: Int): Boolean =
    val (minW, minH) = minImageExtent
    val (maxW, maxH) = maxImageExtent
    width >= minW && width <= maxW && height >= minH && height <= maxH

// Clamp extent to supported bounds.
  def clampExtent(width: Int, height: Int): (Int, Int) =
    val (minW, minH) = minImageExtent
    val (maxW, maxH) = maxImageExtent
    (width.max(minW).min(maxW), height.max(minH).min(maxH))
