package io.computenode.samples.cyfra.slides

import io.computenode.cyfra.dsl.Algebra.given
import io.computenode.cyfra.dsl.Value.{Float32, Int32, Vec4}
import io.computenode.cyfra.dsl.given

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Paths
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.compiletime.error
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.given
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Control.when
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.GStruct.Empty
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.utility.ImageUtility

@main
def simpleray =
  val dim = 1024
  val fovDeg = 60

  case class Sphere(
    center: Vec3[Float32],
    radius: Float32,
    color: Vec3[Float32],
    emissive: Vec3[Float32],
  ) extends GStruct[Sphere]

  def getColorForRay(rayPos: Vec3[Float32], rayDirection: Vec3[Float32]): Vec4[Float32] =
    val sphereCenter = (0f, 0.5f, 3f)
    val sphereRadius = 1f
    val toRay = rayPos - sphereCenter
    val b = toRay dot rayDirection
    val c = (toRay dot toRay) - (sphereRadius * sphereRadius)
    when((c < 0f || b < 0f) && b * b - c > 0f) {
      (1f, 1f, 1f, 1f)
    } otherwise {
      (0f, 0f, 0f, 1f)
    }

  val raytracing: GFunction[Empty, Vec4[Float32], Vec4[Float32]] = GFunction(dim, dim, {
    case (_, (xi: Int32, yi: Int32), _) =>
      val x = (xi.asFloat / dim.toFloat) * 2f - 1f
      val y = (yi.asFloat / dim.toFloat) * 2f - 1f

      val rayPosition = (0f, 0f, 0f)
      val cameraDist = 1.0f / tan(fovDeg * 0.6f * math.Pi.toFloat / 180.0f)
      val rayTarget = (x, y, cameraDist)

      val rayDir = normalize(rayTarget - rayPosition)
      getColorForRay(rayPosition, rayDir)
  })


  val mem = Vec4FloatMem(Array.fill(dim * dim)((0f,0f,0f,0f)))
  val result = Await.result(mem.map(raytracing), 1.second)
  ImageUtility.renderToImage(result, dim, Paths.get(s"generated2.png"))
