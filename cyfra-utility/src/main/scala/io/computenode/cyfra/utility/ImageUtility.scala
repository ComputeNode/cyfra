package io.computenode.cyfra.utility

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Path
import javax.imageio.ImageIO

object ImageUtility {
  def renderToImage(arr: Array[(Float, Float, Float, Float)], n: Int, location: Path): Unit = renderToImage(arr, n, n, location) 
  def renderToImage(arr: Array[(Float, Float, Float, Float)], w: Int, h: Int, location: Path): Unit = {
    val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        val (r,g,b, _) = arr(y * w + x)
        def clip(f: Float) = Math.min(1.0f, Math.max(0.0f, f))
        val (iR, iG, iB) = ((clip(r) * 255).toInt, (clip(g) * 255).toInt, (clip(b) * 255).toInt)
        image.setRGB(x, y, (iR << 16) | (iG << 8) | iB)
      }
    }

    val outputFile = location.toFile
    ImageIO.write(image, "png", outputFile)
  }

  def loadImage(path: Path): Array[(Float, Float, Float, Float)] = {
    val image = ImageIO.read(path.toFile)
    val w = image.getWidth
    val h = image.getHeight
    val arr = Array.fill[(Float, Float, Float, Float)](w * h)((0f, 0f, 0f, 1f))
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        val rgb = image.getRGB(x, y)
        val r = ((rgb >> 16) & 0xFF) / 255.0f
        val g = ((rgb >> 8) & 0xFF) / 255.0f
        val b = (rgb & 0xFF) / 255.0f
        arr(y * w + x) = (r, g, b, 1.0f)
      }
    }
    arr
  }
}
