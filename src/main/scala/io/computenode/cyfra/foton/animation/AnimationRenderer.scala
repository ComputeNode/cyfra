package io.computenode.cyfra.foton.animation

import io.computenode.cyfra
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.{GFunction, RGBA}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.rt.ImageRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.animation.AnimationRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.RtRenderer
import io.computenode.cyfra.foton.rt.animation.AnimatedScene
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.{*, given}

import java.nio.file.{Path, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

trait AnimationRenderer[S <: AnimationRenderer.Scene, F <: GFunction[_, Vec4[Float32], Vec4[Float32]]](params: AnimationRenderer.Parameters):

  private val msPerFrame = 1000.0f / params.framesPerSecond

  def renderFramesToDir(scene: S, destinationPath: Path): Unit =
    destinationPath.toFile.mkdirs()
    val images = renderFrames(scene)
    val totalFrames = Math.ceil(scene.duration.toFloat / msPerFrame).toInt
    val requiredDigits = Math.ceil(Math.log10(totalFrames)).toInt
    images.zipWithIndex.foreach:
      case (image, i) =>
        val frameFormatted = i.toString.reverse.padTo(requiredDigits, '0').reverse.mkString
        val destionationFile = destinationPath.resolve(s"frame$frameFormatted.png")
        ImageUtility.renderToImage(image, params.width, params.height, destionationFile)

  def renderFrames(scene: S): LazyList[Array[RGBA]] =
    val function = renderFunction(scene)
    val totalFrames = Math.ceil(scene.duration.toFloat / msPerFrame).toInt
    val timestamps = LazyList.range(0, totalFrames).map(_ * msPerFrame)
    timestamps.zipWithIndex.map {
      case (time, frame) =>
        timed(s"Animated frame ${frame}/${totalFrames}"):
          renderFrame(scene, time, function)
    }

  protected def renderFrame(
    scene: S,
    time: Float32,
    fn: F
  ): Array[RGBA]

  protected def renderFunction(scene: S): F

object AnimationRenderer:
  trait Parameters:
    def width: Int
    def height: Int
    def framesPerSecond: Int

  trait Scene:
    def duration: Milliseconds
