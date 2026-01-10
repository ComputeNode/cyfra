package io.computenode.cyfra.foton.animation

import io.computenode.cyfra
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.foton.ImageUtility
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.utility.Utility.timed

import java.nio.file.Path

trait AnimationRenderer[S <: AnimationRenderer.Scene, F <: GFunction[?, Vec4[Float32], Vec4[Float32]]](params: AnimationRenderer.Parameters):

  private val msPerFrame = 1000.0f / params.framesPerSecond

  def renderFramesToDir(scene: S, destinationPath: Path): Unit =
    destinationPath.toFile.mkdirs()
    val function = renderFunction(scene)
    val totalFrames = Math.ceil(scene.duration / msPerFrame).toInt
    val requiredDigits = Math.max(1, Math.ceil(Math.log10(totalFrames)).toInt)
    try
      for frame <- 0 until totalFrames do
        val time = frame * msPerFrame
        val image = timed(s"Animated frame $frame/$totalFrames"):
          renderFrame(scene, time, function)
        val frameFormatted = frame.toString.reverse.padTo(requiredDigits, '0').reverse.mkString
        val destinationFile = destinationPath.resolve(s"frame$frameFormatted.png")
        ImageUtility.renderToImage(image, params.width, params.height, destinationFile)
    finally
      close()
  
  /** Override to clean up resources after rendering */
  def close(): Unit = ()

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
