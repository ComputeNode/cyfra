package io.computenode.cyfra.foton.rt

import io.computenode.cyfra
import ImageRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.RtRenderer.RayHitInfo
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.foton.rt.ImageRtRenderer
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.rt.shapes.{Box, Sphere}
import io.computenode.cyfra.dsl.{GStruct, UniformContext, given}
import io.computenode.cyfra.runtime.GFunction
import io.computenode.cyfra.runtime.mem.GMem.fRGBA
import io.computenode.cyfra.utility.ImageUtility
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.dsl.Algebra.{*, given}

import java.nio.file.{Path, Paths}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

class ImageRtRenderer(params: ImageRtRenderer.Parameters) extends RtRenderer(params):

  def renderToFile(scene: Scene, destinationPath: Path): Unit =
    val images = render(scene)
    for image <- images do ImageUtility.renderToImage(image, params.width, params.height, destinationPath)

  def render(scene: Scene): LazyList[Array[fRGBA]] =
    render(scene, renderFunction(scene))

  private def render(scene: Scene, fn: GFunction[RaytracingIteration, Vec4[Float32], Vec4[Float32]]): LazyList[Array[fRGBA]] =
    val initialMem = Array.fill(params.width * params.height)((0.5f, 0.5f, 0.5f, 0.5f))
    LazyList
      .iterate((initialMem, 0), params.iterations + 1) { case (mem, render) =>
        UniformContext.withUniform(RaytracingIteration(render)):
          val fmem = Vec4FloatMem(mem)
          val result = timed(s"Rendered iteration $render")(fmem.map(fn).asInstanceOf[Vec4FloatMem].toArray)
          (result, render + 1)
      }
      .drop(1)
      .map(_._1)

  private def renderFunction(scene: Scene): GFunction[RaytracingIteration, Vec4[Float32], Vec4[Float32]] =
    GFunction.from2D(params.width):
      case (RaytracingIteration(frame), (xi: Int32, yi: Int32), lastFrame) =>
        renderFrame(xi, yi, frame, lastFrame, scene)

object ImageRtRenderer:

  private case class RaytracingIteration(frame: Int32) extends GStruct[RaytracingIteration]

  case class Parameters(
    width: Int,
    height: Int,
    fovDeg: Float = 60.0f,
    superFar: Float = 1000.0f,
    maxBounces: Int = 8,
    pixelIterations: Int = 1000,
    iterations: Int = 5,
    bgColor: (Float, Float, Float) = (0.2f, 0.2f, 0.2f),
  ) extends RtRenderer.Parameters
