package io.computenode.cyfra.satellite.visualization

import io.computenode.cyfra.satellite.data.*
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO
import scala.util.{Try, Success, Failure}

/** Utilities for exporting spectral index results as PNG images */
object ImageExporter:

  /** Export a color map legend as a horizontal bar
    *
    * @param outputPath Path to save the legend PNG
    * @param colorMap Color mapping function
    * @param min Minimum value
    * @param max Maximum value
    * @param width Legend width in pixels
    * @param height Legend height in pixels
    */
  def exportLegend(
      outputPath: Path,
      colorMap: (Float, Float, Float) => ColorMaps.RGB,
      min: Float,
      max: Float,
      width: Int = 400,
      height: Int = 50
  ): Try[Unit] = Try {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for
      y <- 0 until height
      x <- 0 until width
    do
      val value = min + (max - min) * x / width.toFloat
      val (r, g, b) = colorMap(value, min, max)
      val rgb = (r << 16) | (g << 8) | b
      image.setRGB(x, y, rgb)

    Files.createDirectories(outputPath.getParent)
    ImageIO.write(image, "png", outputPath.toFile)
  }
  

  /** Export a spectral index result as a color-mapped PNG image
    *
    * @param result The spectral index result to export
    * @param outputPath Path to save the PNG file
    * @param colorMap Color mapping function (value, min, max) => (R, G, B)
    * @param valueRange Optional custom value range (min, max). If None, uses result statistics
    * @return Success or error message
    */
  def exportAsPNG(
      result: SpectralIndexResult,
      outputPath: Path,
      colorMap: (Float, Float, Float) => ColorMaps.RGB = ColorMaps.viridis,
      valueRange: Option[(Float, Float)] = None
  ): Try[Unit] = Try {
    val (min, max) = valueRange.getOrElse {
      val stats = result.statistics
      (stats.min, stats.max)
    }

    val image = new BufferedImage(result.width, result.height, BufferedImage.TYPE_INT_RGB)

    // Convert each pixel value to RGB color
    for
      y <- 0 until result.height
      x <- 0 until result.width
    do
      val idx = y * result.width + x
      val value = result.values(idx)
      
      val (r, g, b) = if (value.isNaN || value.isInfinity) {
        (0, 0, 0) // Black for invalid values
      } else {
        colorMap(value, min, max)
      }
      
      val rgb = (r << 16) | (g << 8) | b
      image.setRGB(x, y, rgb)

    // Ensure parent directory exists
    Files.createDirectories(outputPath.getParent)
    
    // Write the image
    ImageIO.write(image, "png", outputPath.toFile)
  }

  /** Export a spectral index result with automatic color map selection
    *
    * Chooses appropriate color map based on the index name
    */
  def exportWithAutoColorMap(
      result: SpectralIndexResult,
      outputPath: Path
  ): Try[Unit] =
    val colorMap = ColorMaps.forIndex(result.indexName)
    exportAsPNG(result, outputPath, colorMap)

  /** Export a false color composite from multiple bands
    *
    * @param bands Three spectral index results or bands to use as R, G, B channels
    * @param outputPath Path to save the PNG file
    * @param stretch Optional histogram stretch for better contrast
    * @return Success or error message
    */
  def exportFalseColor(
      bands: (SpectralIndexResult, SpectralIndexResult, SpectralIndexResult),
      outputPath: Path,
      stretch: Option[(Float, Float)] = None
  ): Try[Unit] = Try {
    val (red, green, blue) = bands
    
    require(
      red.width == green.width && green.width == blue.width,
      "All bands must have same width"
    )
    require(
      red.height == green.height && green.height == blue.height,
      "All bands must have same height"
    )

    val image = new BufferedImage(red.width, red.height, BufferedImage.TYPE_INT_RGB)

    // Compute min/max for each band for stretching
    val redStats = red.statistics
    val greenStats = green.statistics
    val blueStats = blue.statistics

    val (stretchMin, stretchMax) = stretch.getOrElse {
      val globalMin = Math.min(redStats.min, Math.min(greenStats.min, blueStats.min))
      val globalMax = Math.max(redStats.max, Math.max(greenStats.max, blueStats.max))
      (globalMin, globalMax)
    }

    def normalize(value: Float): Int =
      if (value.isNaN || value.isInfinity) 0
      else {
        val normalized = (value - stretchMin) / (stretchMax - stretchMin)
        (Math.max(0.0f, Math.min(1.0f, normalized)) * 255).toInt
      }

    // Convert each pixel
    for
      y <- 0 until red.height
      x <- 0 until red.width
    do
      val idx = y * red.width + x
      val r = normalize(red.values(idx))
      val g = normalize(green.values(idx))
      val b = normalize(blue.values(idx))
      
      val rgb = (r << 16) | (g << 8) | b
      image.setRGB(x, y, rgb)

    // Write the image
    Files.createDirectories(outputPath.getParent)
    ImageIO.write(image, "png", outputPath.toFile)
  }

  /** Export a satellite image as a true color or false color composite
    *
    * @param image Source satellite image with band data
    * @param outputPath Path to save the PNG file
    * @param bandMapping Triple of band names to use as (R, G, B)
    * @param stretch Optional stretch range for contrast enhancement
    */
  def exportComposite(
      image: SatelliteImage,
      outputPath: Path,
      bandMapping: (String, String, String),
      stretch: Option[(Float, Float)] = None
  ): Try[Unit] = Try {
    val (redBand, greenBand, blueBand) = bandMapping
    
    require(image.hasBand(redBand), s"Image missing band: $redBand")
    require(image.hasBand(greenBand), s"Image missing band: $greenBand")
    require(image.hasBand(blueBand), s"Image missing band: $blueBand")

    val bufferedImage = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_RGB)

    val red = image.bands(redBand)
    val green = image.bands(greenBand)
    val blue = image.bands(blueBand)

    // Auto-compute stretch if not provided
    val (min, max) = stretch.getOrElse {
      val allValues = red ++ green ++ blue
      val validValues = allValues.filter(v => !v.isNaN && !v.isInfinity)
      if (validValues.isEmpty) (0.0f, 1.0f)
      else {
        // Use 2nd and 98th percentile for better contrast
        val sorted = validValues.sorted
        val minIdx = (sorted.length * 0.02).toInt
        val maxIdx = (sorted.length * 0.98).toInt
        (sorted(minIdx), sorted(maxIdx))
      }
    }

    def normalize(value: Float): Int =
      if (value.isNaN || value.isInfinity) 0
      else {
        val normalized = (value - min) / (max - min)
        (Math.max(0.0f, Math.min(1.0f, normalized)) * 255).toInt
      }

    // Convert each pixel
    for
      y <- 0 until image.height
      x <- 0 until image.width
    do
      val idx = y * image.width + x
      val r = normalize(red(idx))
      val g = normalize(green(idx))
      val b = normalize(blue(idx))
      
      val rgb = (r << 16) | (g << 8) | b
      bufferedImage.setRGB(x, y, rgb)

    // Write the image
    Files.createDirectories(outputPath.getParent)
    ImageIO.write(bufferedImage, "png", outputPath.toFile)
  }

  /** Create a side-by-side comparison of two results
    *
    * Useful for before/after change detection visualization
    */
  def exportComparison(
      left: SpectralIndexResult,
      right: SpectralIndexResult,
      outputPath: Path,
      colorMap: (Float, Float, Float) => ColorMaps.RGB = ColorMaps.viridis,
      separator: Int = 10 // pixels between images
  ): Try[Unit] = Try {
    require(
      left.width == right.width && left.height == right.height,
      "Images must have same dimensions"
    )

    val totalWidth = left.width * 2 + separator
    val image = new BufferedImage(totalWidth, left.height, BufferedImage.TYPE_INT_RGB)

    // Compute shared value range
    val leftStats = left.statistics
    val rightStats = right.statistics
    val min = Math.min(leftStats.min, rightStats.min)
    val max = Math.max(leftStats.max, rightStats.max)

    // Render left image
    for
      y <- 0 until left.height
      x <- 0 until left.width
    do
      val idx = y * left.width + x
      val value = left.values(idx)
      val (r, g, b) = if (value.isNaN || value.isInfinity) (0, 0, 0)
                      else colorMap(value, min, max)
      val rgb = (r << 16) | (g << 8) | b
      image.setRGB(x, y, rgb)

    // Render separator (white)
    for
      y <- 0 until left.height
      x <- left.width until (left.width + separator)
    do
      image.setRGB(x, y, 0xFFFFFF)

    // Render right image
    for
      y <- 0 until right.height
      x <- 0 until right.width
    do
      val idx = y * right.width + x
      val value = right.values(idx)
      val (r, g, b) = if (value.isNaN || value.isInfinity) (0, 0, 0)
                      else colorMap(value, min, max)
      val rgb = (r << 16) | (g << 8) | b
      image.setRGB(left.width + separator + x, y, rgb)

    // Write the image
    Files.createDirectories(outputPath.getParent)
    ImageIO.write(image, "png", outputPath.toFile)
  }


