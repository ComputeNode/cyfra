package io.computenode.cyfra.juliaset

import io.computenode.cyfra.dsl.Value.*

import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.{Await, ExecutionContext}
import io.computenode.cyfra.dsl.Algebra.*
import io.computenode.cyfra.dsl.Algebra.given

import scala.concurrent.duration.DurationInt
import io.computenode.cyfra.*
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.dsl.{Empty, GFunction, GContext, GSeq, MVPContext, Vec4FloatMem}
import io.computenode.cyfra.{ImageTests, ImageUtility}
import org.apache.commons.io.IOUtils
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.runner.RunWith

import java.io.File
import java.nio.file.Files

class JuliaSet {
  given GContext = new MVPContext()
  given ExecutionContext = Implicits.global

  @Test
  def testJuliaSet(): Unit = {
    val dim = 4096
    val max = 1
    val RECURSION_LIMIT = 1000
    val const = (0.355f, 0.355f)

    val function: GFunction[Empty, Vec4[Float32], Vec4[Float32]] = GFunction(dim, dim, {
      case (_, (xi: Int32, yi: Int32), _) =>
        val x = 3.0f * (xi - (dim / 2)).asFloat / dim.toFloat
        val y = 3.0f * (yi - (dim / 2)).asFloat / dim.toFloat
        val uv = (x, y)
        val len = length((x, y)) * 1000f

        def juliaSet(uv: Vec2[Float32]): Int32 =
          GSeq.gen(uv, next = v => {
            ((v.x * v.x) - (v.y * v.y), 2.0f * v.x * v.y) + const
          }).limit(RECURSION_LIMIT).map(length).takeWhile(_ < 2.0f).count

        def rotate(uv: Vec2[Float32], angle: Float32): Vec2[Float32] =
          val newXAxis = (cos(angle), sin(angle))
          val newYAxis = (-newXAxis.y, newXAxis.x)
          (uv dot newXAxis, uv dot newYAxis) * 0.9f


        def interpolateColor(f: Float32): Vec3[Float32] =
          val c1 = (8f, 22f, 104f) * (1 / 255f)
          val c2 = (62f, 82f, 199f) * (1 / 255f)
          val c3 = (221f, 233f, 255f) * (1 / 255f)
          val ratio1 = (1f - f) * (1f - f)
          val ratio2 = 2f * f * (1f - f)
          val ratio3 = f * f
          c1 * ratio1 + c2 * ratio2 + c3 * ratio3

        val angle = Math.PI.toFloat / 3.0f
        val rotatedUv = rotate(uv, angle)

        val recursionCount = juliaSet(rotatedUv)

        val f = recursionCount.asFloat / 100f
        val ff = when(f > 1f)(1f).otherwise(f)

        when(recursionCount > 20):
          val color = interpolateColor(ff)
          (
            color.r,
            color.g,
            color.b,
            1.0f
          )
        .otherwise:
          (8f / 255f, 22f / 255f, 104f / 255f, 1.0f)
    })

    val r = Await.result(Vec4FloatMem(dim * dim).map(function), 10.hours)
    val outputTemp = File.createTempFile("julia", ".png")
    ImageUtility.renderToImage(r, dim, outputTemp.toPath)
    val referenceImage = getClass.getResource("julia.png")
    ImageTests.assertImagesEquals(outputTemp, new File(referenceImage.getPath))
  }
}

