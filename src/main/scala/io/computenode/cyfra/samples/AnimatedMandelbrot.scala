package io.computenode.cyfra.samples.foton

import java.io.File
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.GSeq
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.animation.AnimatedFunctionRenderer.Parameters
import io.computenode.cyfra.foton.animation.{AnimatedFunction, AnimatedFunctionRenderer}
import io.computenode.cyfra.foton.animation.AnimationFunctions.{AnimationInstant, smooth}
import io.computenode.cyfra.utility.Color.*
import io.computenode.cyfra.utility.Math3D.*
import io.computenode.cyfra.utility.Units.Milliseconds 

import scala.concurrent.duration.DurationInt
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

object AnimatedMandelbrot:
  // Animation parameters
  private val AnimationDuration = 10.seconds
  private val FramesPerSecond = 60
  // Small batch size prevents memory overflow
  // and distributes CPU load across time instead of all at once
  private val BatchSize = 8
  private val ImageWidth = 1024
  private val ImageHeight = 1024
  
  // Mandelbrot parameters
  private val BaseZoom = 1.0f
  private val TargetZoom = 50.0f
  private val MandelbrotCenterX = -0.743643887037151f
  private val MandelbrotCenterY = 0.231825904205330f
  private val InitialFocusX = 0.0f
  private val InitialFocusY = 1.25f
  private val IterationLimit = 80000
  
  /**
   * Finds the last rendered frame in a directory to enable resuming animation renders
   */
  def findLastRenderedFrame(directory: Path): Int =
    val dir = directory.toFile
    if !dir.exists() then
      dir.mkdirs()
      return 0
    
    val framePattern = "frame(\\d+)\\.png".r
    
    Option(dir.listFiles())
      .getOrElse(Array.empty[File])
      .filter(_.isFile)
      .filter(_.getName.endsWith(".png"))
      .flatMap { file => 
        val name = file.getName
        framePattern.findFirstMatchIn(name).map(_.group(1).toInt)
      } match
        case frames if frames.isEmpty => 0
        case frames => frames.max + 1
  
  /**
   * Calculates the Mandelbrot set iteration count at a given point with animation parameters
   */
  def calculateMandelbrot(c: Vec2[Float32], globalTimePos: Float32): Int32 =
    // Calculate zoom factor based on animation time
    val zoom = BaseZoom + (TargetZoom - BaseZoom) * globalTimePos
    
    // Calculate focus point based on animation time
    val focusX = mix(InitialFocusX, MandelbrotCenterX, globalTimePos)
    val focusY = mix(InitialFocusY, MandelbrotCenterY, globalTimePos)
    
    // Scale coordinates based on zoom and focus
    val scaledC = vec2(
      (c.x - 0.3f) * 2.5f / zoom + focusX,
      (c.y - 0.5f) * 2.5f / zoom + focusY
    )
    
    // Calculate iteration count using Z = Z² + C
    // Using GSeq with lazy evaluation to avoid memory explosion
    // Each iteration is computed on-demand instead of materializing the entire sequence
    GSeq
      .gen(vec2(0f, 0f), next = z => ((z.x * z.x) - (z.y * z.y), 2.0f * z.x * z.y) + scaledC)
      .limit(IterationLimit)
      .map(length)
      .takeWhile(_ < 2.0f)
      .count

  /**
   * Creates a function that produces the animated Mandelbrot visualization
   */
  def createMandelbrotFunction(batchStartTime: Float32, 
                               batchDuration: Milliseconds,
                               animationDurationMs: Float32): AnimatedFunction =
    def mandelbrotColor(uv: Vec2[Float32])(using instant: AnimationInstant): Vec4[Float32] =
      // Calculate global animation position
      val globalTimePos = (instant.time + batchStartTime) / animationDurationMs
      
      // Get iteration count for this coordinate
      val recursionCount = calculateMandelbrot(uv, globalTimePos)
      
      // Normalize iteration count based on zoom level
      val normalizer = 300f * (1f + 0.1f * globalTimePos)
      val colorPos = min(1f, recursionCount.asFloat / normalizer)
      
      // Apply color mapping
      val color = interpolate(InterpolationThemes.Blue, colorPos)
      (color.r, color.g, color.b, 1.0f)

    AnimatedFunction.fromCoord(mandelbrotColor, batchDuration)

  /**
   * Processes a batch of rendered frames, moving them to final location with correct names
   */
  def processBatchFrames(batchDir: Path, outputDir: Path, batchStart: Int, totalFrames: Int): Unit =
    val frameFiles = Option(batchDir.toFile.listFiles())
      .getOrElse(Array.empty[File])
      .filter(_.getName.endsWith(".png"))
      .sorted
      
    val frameDigits = totalFrames.toString.length
    
    frameFiles.zipWithIndex.foreach { case (file, i) =>
      val frameNum = batchStart + i
      val frameFormatted = frameNum.toString.padLeft(frameDigits, '0')
      val destFile = outputDir.resolve(s"frame$frameFormatted.png")
      Files.move(file.toPath, destFile, StandardCopyOption.REPLACE_EXISTING)
    }
    
    batchDir.toFile.delete()

  // String extension method for padding
  extension (s: String) 
    def padLeft(len: Int, padChar: Char): String = 
      s.reverse.padTo(len, padChar).reverse.mkString

  @main
  def mandelbrot(): Unit =
    val animationDurationMs = AnimationDuration.toMillis.toFloat
    val totalFrames = (animationDurationMs / 1000f * FramesPerSecond).toInt
    
    val outputDir = Paths.get("mandelbrot")
    // Resume capability prevents redundant work if previous run crashed
    val startFrame = findLastRenderedFrame(outputDir)
    
    println(s"Starting from frame $startFrame of $totalFrames")
    
    // Process in batches to manage memory and prevent CPU overload
    for 
      batchStart <- startFrame until totalFrames by BatchSize
      batchEnd = Math.min(batchStart + BatchSize, totalFrames)
    do
      println(s"Rendering batch from $batchStart to ${batchEnd-1}")
      
      // Calculate timing for this batch
      val batchDuration = AnimationDuration * ((batchEnd - batchStart).toFloat / totalFrames)
      val startTime = animationDurationMs * (batchStart.toFloat / totalFrames)
      
      // Create temporary output directory for batch
      val batchDir = outputDir.resolve(s"batch_${batchStart}_to_${batchEnd-1}")
      batchDir.toFile.mkdirs()
      
      // Create and render the animation function
      val mandelbrotFunction = createMandelbrotFunction(
        startTime, 
        batchDuration, 
        animationDurationMs
      )
      
      val batchParameters = Parameters(ImageWidth, ImageHeight, FramesPerSecond)
      val batchRenderer = AnimatedFunctionRenderer(batchParameters)
      batchRenderer.renderFramesToDir(mandelbrotFunction, batchDir)
      
      // Process generated frames
      processBatchFrames(batchDir, outputDir, batchStart, totalFrames)
      
      // Force garbage collection between batches to prevent memory leaks
      System.gc()