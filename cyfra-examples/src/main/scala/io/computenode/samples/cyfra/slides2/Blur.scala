package io.computenode.samples.cyfra.slides2

import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.runtime.mem.GMem.fRGBA
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.runtime.{GContext, GFunction}
import io.computenode.cyfra.utility.ImageUtility
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.samples.cyfra.slides2.SlidesUtility.saveImage

import java.nio.file.Paths

object Blur:

  val dim = 1024

  given GContext = GContext()

  type BlurFunction = GFunction[GStruct.Empty, Vec4[Float32], Vec4[Float32]]

  def blurFunction(radius: Int): BlurFunction =
    GFunction.from2D(width = dim):
      case (_, (x, y), image) =>

        def sample(dx: Int, dy: Int): Vec4[Float32] =
          image.at(x + dx, y + dy)

        def blur(radius: Int): Vec4[Float32] =
          val samples = for
            dx <- -radius to radius
            dy <- -radius to radius
          yield sample(dx, dy)
          val weight = 1.0f / samples.length
          samples.reduce(_ + _) * weight

        blur(radius)

  def scalaBlur(radius: Int, image: Array[Array[fRGBA]]): Array[Array[fRGBA]] =
    val width = image.length
    val height = image(0).length
    val blurred = Array.ofDim[fRGBA](width, height)

    def pixel(x: Int, y: Int): fRGBA =

      def sample(dx: Int, dy: Int): fRGBA =
        if x + dx >= 0 && x + dx < width && y + dy >= 0 && y + dy < height then
          image(x + dx)(y + dy)
        else
          (0.0f, 0.0f, 0.0f, 1.0f) // Out of bounds, return transparent pixel
          
        
      def blur(radius: Int): fRGBA =
        val samples = for
          dx <- -radius to radius
          dy <- -radius to radius
        yield sample(dx, dy)
        val count = samples.length
        val (r, g, b, a) = samples.reduce((acc, p) => (acc._1 + p._1, acc._2 + p._2, acc._3 + p._3, acc._4 + p._4))
        (r / count, g / count, b / count, a / count)
    
      blur(radius)

    for x <- 0 until width do
      for y <- 0 until height do
        blurred(x)(y) = pixel(x, y)
    blurred

  @main
  def blurLambdaDays =
    val file = Paths.get("lambda_days.png")
    val image = ImageUtility.loadImage(file)
    val data = Vec4FloatMem(image)

    val output = data.map(blurFunction(radius = 20))

    saveImage(output, dim, dim, Paths.get("blurred_lambda_days.png"))

  @main
  def scalaBlurLambdaDays =
    val file = Paths.get("lambda_days.png")
    val image = ImageUtility.loadImage(file)
    val width = 1024
    val array2d = image.zipWithIndex.groupBy( _._2 / width ).values.map(_.map(_._1)).toArray

//    scalaBlur(radius = 20, array2d)
//    scalaBlur(radius = 20, array2d)
//    scalaBlur(radius = 20, array2d)
//    scalaBlur(radius = 20, array2d)

    val blurred = timed("Scala blur"):
      scalaBlur(radius = 20, array2d)

