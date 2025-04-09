package io.computenode.cyfra

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Path
import javax.imageio.ImageIO
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.ImageIcon
import java.awt.Component

object ImageUtility {
  def renderToImage(arr: Array[(Float, Float, Float, Float)], n: Int, location: Path): Unit = renderToImage(arr, n, n, location) 
  def renderBufferedImg(arr: Array[(Float, Float, Float, Float)], w: Int, h: Int): BufferedImage = {
    val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        val (r,g,b, _) = arr(y * w + x)
        def clip(f: Float) = Math.min(1.0f, Math.max(0.0f, f))
        val (iR, iG, iB) = ((clip(r) * 255).toInt, (clip(g) * 255).toInt, (clip(b) * 255).toInt)
        image.setRGB(x, y, (iR << 16) | (iG << 8) | iB)
      }
    }
    image
  }

  def renderToImage(arr: Array[(Float, Float, Float, Float)], w: Int, h: Int, location: Path): Unit = {
    val image = renderBufferedImg(arr, w, h)
    val outputFile = location.toFile
    ImageIO.write(image, "png", outputFile)
  }

  def displayImageToWindow(image: BufferedImage, frame: JFrame): Unit = {
    frame.getContentPane.getComponents.collectFirst { case label: JLabel =>
      label.setIcon(new ImageIcon(image))
    }
    frame.repaint()
  }
}
