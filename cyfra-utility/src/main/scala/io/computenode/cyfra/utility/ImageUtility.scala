package io.computenode.cyfra.utility

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Path
import javax.imageio.ImageIO

object ImageUtility:
  def renderToImage(arr: Array[(Float, Float, Float, Float)], n: Int, location: Path): Unit = renderToImage(arr, n, n, location)
  def renderToImage(arr: Array[(Float, Float, Float, Float)], w: Int, h: Int, location: Path): Unit =
    val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    for y <- 0 until h do
      for x <- 0 until w do
        val (r, g, b, _) = arr(y * w + x)
        def clip(f: Float) = Math.min(1.0f, Math.max(0.0f, f))
        val (iR, iG, iB) = ((clip(r) * 255).toInt, (clip(g) * 255).toInt, (clip(b) * 255).toInt)
        image.setRGB(x, y, (iR << 16) | (iG << 8) | iB)

    val outputFile = location.toFile
    ImageIO.write(image, "png", outputFile)
