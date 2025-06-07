package io.computenode.samples.cyfra.slides2

import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.runtime.{GContext, GFunction}
import io.computenode.cyfra.utility.ImageUtility
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

  @main
  def blurLambdaDays =
    val file = Paths.get("lambda_days.png")
    val image = ImageUtility.loadImage(file)
    val data = Vec4FloatMem(image)

    val output = data.map(blurFunction(radius = 20))

    saveImage(output, dim, dim, Paths.get("blurred_lambda_days.png"))