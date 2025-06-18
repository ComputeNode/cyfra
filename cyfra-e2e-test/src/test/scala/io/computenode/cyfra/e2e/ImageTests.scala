package io.computenode.cyfra.e2e

import com.diogonunes.jcolor.Ansi.colorize
import com.diogonunes.jcolor.Attribute
import munit.Assertions.assertEquals

import java.awt.Image
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object ImageTests:
  def assertImagesEquals(result: File, expected: File) =
    val expectedImage = ImageIO.read(expected)
    val resultImage = ImageIO.read(result)
    // println("Got image:")
    // println(renderAsText(resultImage, 50, 50))
    assertEquals(expectedImage.getWidth, resultImage.getWidth, "Width was different")
    assertEquals(expectedImage.getHeight, resultImage.getHeight, "Height was different")
    for {
      x <- 0 until expectedImage.getWidth
      y <- 0 until expectedImage.getHeight
    }
      val equal = expectedImage.getRGB(x, y) == resultImage.getRGB(x, y)
      assert(equal, s"Pixel $x, $y was different. Output file: ${result.getAbsolutePath}")

  def renderAsText(bufferedImage: BufferedImage, w: Int, h: Int) =
    val downscaled = bufferedImage.getScaledInstance(w, h, Image.SCALE_SMOOTH)
    val scaledImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val graphics = scaledImage.createGraphics()
    graphics.drawImage(downscaled, 0, 0, null)
    graphics.dispose()

    val imageText = new StringBuilder
    for y <- 0 until h do
      for x <- 0 until w do
        val rgb = scaledImage.getRGB(x, y)
        val r = (rgb >> 16) & 0xff
        val g = (rgb >> 8) & 0xff
        val b = rgb & 0xff
        val pixel = colorize("   ", Attribute.BACK_COLOR(r, g, b))
        imageText.append(pixel)
      imageText.append("\n")
    imageText.toString
