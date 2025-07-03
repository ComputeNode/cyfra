package io.computenode.cyfra.foton.animation

import io.computenode.cyfra
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.GFunction
import io.computenode.cyfra.runtime.mem.GMem.fRGBA
import io.computenode.cyfra.utility.ImageUtility
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.utility.Utility.timed

import java.nio.file.Path

trait AnimationRenderer[S <: AnimationRenderer.Scene, F <: GFunction[?, Vec4[Float32], Vec4[Float32]]](params: AnimationRenderer.Parameters):

  private val msPerFrame = 1000.0f / params.framesPerSecond

  def renderFramesToDir(scene: S, destinationPath: Path): Unit =
    destinationPath.toFile.mkdirs()
    val images = renderFrames(scene)
    val totalFrames = Math.ceil(scene.duration / msPerFrame).toInt
    val requiredDigits = Math.ceil(Math.log10(totalFrames)).toInt
    images.zipWithIndex.foreach:
      case (image, i) =>
        val frameFormatted = i.toString.reverse.padTo(requiredDigits, '0').reverse.mkString
        val destionationFile = destinationPath.resolve(s"frame$frameFormatted.png")
        ImageUtility.renderToImage(image, params.width, params.height, destionationFile)

  def renderFrames(scene: S): LazyList[Array[fRGBA]] =
    val function = renderFunction(scene)
    val totalFrames = Math.ceil(scene.duration / msPerFrame).toInt
    val timestamps = LazyList.range(0, totalFrames).map(_ * msPerFrame)
    timestamps.zipWithIndex.map { case (time, frame) =>
      timed(s"Animated frame $frame/$totalFrames"):
        renderFrame(scene, time, function)
    }

  protected def renderFrame(scene: S, time: Float32, fn: F): Array[fRGBA]

  protected def renderFunction(scene: S): F

object AnimationRenderer:
  trait Parameters:
    def width: Int
    def height: Int
    def framesPerSecond: Int

  trait Scene:
    def duration: Milliseconds
