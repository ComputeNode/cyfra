package io.computenode.cyfra.satellite.animation

import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.satellite.spectral.*
import io.computenode.cyfra.satellite.visualization.*
import io.computenode.cyfra.foton.*
import io.computenode.cyfra.foton.animation.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import java.nio.file.{Path, Paths}
import java.time.LocalDateTime
import scala.concurrent.duration.*

/** Time-series animation for satellite data using cyfra-foton
  *
  * Animates changes in spectral indices over time, useful for:
  * - Vegetation growth/decline visualization
  * - Deforestation progression
  * - Urban expansion
  * - Drought evolution
  * - Fire spread and recovery
  */
class TimeSeriesAnimator(using runtime: VkCyfraRuntime):

  /** Create an animated visualization of a spectral index over time
    *
    * @param timeSeries Time series of satellite images
    * @param indexName Name of spectral index to compute (e.g., "NDVI")
    * @param outputDir Directory to save animation frames
    * @param fps Frames per second for animation
    * @param totalDuration Total animation duration
    * @return Sequence of frame paths
    */
  def animateSpectralIndex(
      timeSeries: ImageTimeSeries,
      indexName: String,
      outputDir: Path,
      fps: Int = 30,
      totalDuration: Duration = 10.seconds
  ): Seq[Path] =
    val analyzer = new SpectralAnalyzer()
    val sorted = timeSeries.sorted

    // Compute spectral index for all images
    println(s"Computing $indexName for ${sorted.length} images...")
    val indexResults = sorted.map { case (date, image) =>
      val result = indexName.toUpperCase match
        case "NDVI" => analyzer.computeNDVI(image)
        case "EVI" => analyzer.computeEVI(image)
        case "NDWI" => analyzer.computeNDWI(image)
        case _ => throw new IllegalArgumentException(s"Unsupported index: $indexName")
      date -> result
    }

    // Find global min/max for consistent color scaling
    val allValues = indexResults.flatMap(_._2.values)
    val globalMin = allValues.min
    val globalMax = allValues.max
    println(s"Value range: [$globalMin, $globalMax]")

    // Generate frames with smooth interpolation
    val totalFrames = (fps * totalDuration.toSeconds).toInt
    println(s"Generating $totalFrames frames at ${fps}fps...")

    val colorMap = ColorMaps.forIndex(indexName)

    val frames = (0 until totalFrames).map { frameIdx =>
      val progress = frameIdx.toFloat / (totalFrames - 1)
      
      // Interpolate between time series images
      val imageIdx = progress * (sorted.length - 1)
      val idx1 = imageIdx.toInt
      val idx2 = math.min(idx1 + 1, sorted.length - 1)
      val t = imageIdx - idx1

      val (date1, result1) = indexResults(idx1)
      val (date2, result2) = indexResults(idx2)

      // Linear interpolation between frames
      val interpolatedValues = result1.values.zip(result2.values).map { case (v1, v2) =>
        v1 * (1 - t) + v2 * t
      }

      val interpolatedResult = SpectralIndexResult(
        values = interpolatedValues,
        width = result1.width,
        height = result1.height,
        indexName = indexName,
        sourceMetadata = result1.sourceMetadata
      )

      // Export frame
      val framePath = outputDir.resolve(f"frame${frameIdx}%04d.png")
      ImageExporter.exportAsPNG(
        interpolatedResult,
        framePath,
        colorMap,
        Some((globalMin, globalMax))
      ).get

      if (frameIdx % 10 == 0) {
        println(s"  Frame $frameIdx/$totalFrames")
      }

      framePath
    }

    println(s"Animation complete! ${frames.length} frames saved to ${outputDir.toAbsolutePath}")
    frames

  /** Create a side-by-side comparison animation showing before/after
    *
    * @param before First image
    * @param after Second image
    * @param indexName Spectral index to visualize
    * @param outputDir Output directory
    * @param fps Frames per second
    * @param totalDuration Animation duration
    */
  def animateChange(
      before: SatelliteImage,
      after: SatelliteImage,
      indexName: String,
      outputDir: Path,
      fps: Int = 30,
      totalDuration: Duration = 5.seconds
  ): Seq[Path] =
    val analyzer = new SpectralAnalyzer()

    // Compute indices
    val beforeResult = indexName.toUpperCase match
      case "NDVI" => analyzer.computeNDVI(before)
      case "EVI" => analyzer.computeEVI(before)
      case "NDWI" => analyzer.computeNDWI(before)
      case _ => throw new IllegalArgumentException(s"Unsupported index: $indexName")

    val afterResult = indexName.toUpperCase match
      case "NDVI" => analyzer.computeNDVI(after)
      case "EVI" => analyzer.computeEVI(after)
      case "NDWI" => analyzer.computeNDWI(after)
      case _ => throw new IllegalArgumentException(s"Unsupported index: $indexName")

    val totalFrames = (fps * totalDuration.toSeconds).toInt
    val colorMap = ColorMaps.forIndex(indexName)

    // Find shared value range
    val allValues = beforeResult.values ++ afterResult.values
    val min = allValues.min
    val max = allValues.max

    val frames = (0 until totalFrames).map { frameIdx =>
      val progress = frameIdx.toFloat / (totalFrames - 1)
      
      // Smooth transition using ease-in-out
      val t = if (progress < 0.5f) {
        2 * progress * progress
      } else {
        1 - math.pow(-2 * progress + 2, 2).toFloat / 2
      }

      // Interpolate
      val interpolatedValues = beforeResult.values.zip(afterResult.values).map { 
        case (v1, v2) => v1 * (1 - t) + v2 * t
      }

      val interpolatedResult = SpectralIndexResult(
        values = interpolatedValues,
        width = beforeResult.width,
        height = beforeResult.height,
        indexName = s"${indexName}_transition",
        sourceMetadata = beforeResult.sourceMetadata
      )

      val framePath = outputDir.resolve(f"change_frame${frameIdx}%04d.png")
      ImageExporter.exportAsPNG(
        interpolatedResult,
        framePath,
        colorMap,
        Some((min, max))
      ).get

      framePath
    }

    println(s"Change animation complete! ${frames.length} frames saved")
    frames

  /** Create an animated heat map with time overlay
    *
    * Uses cyfra-foton's animation capabilities for smooth temporal interpolation
    */
  def createAnimatedHeatMap(
      timeSeries: ImageTimeSeries,
      indexName: String,
      resolution: (Int, Int) = (1920, 1080),
      duration: Duration = 10.seconds,
      fps: Int = 30
  ): AnimatedFunctionRenderer =
    val (width, height) = resolution
    
    // This would integrate with cyfra-foton's AnimatedFunction
    // for GPU-accelerated frame rendering
    // For now, we return a basic renderer structure
    
    throw new NotImplementedError(
      "Full cyfra-foton integration coming soon. " +
      "Use animateSpectralIndex() for basic time-series animation."
    )

/** Utilities for creating animated GIFs and videos from frame sequences */
object AnimationExport:

  /** Instructions for creating animated GIF from frames
    *
    * Requires ImageMagick or ffmpeg
    */
  def gifInstructions(frameDir: Path, outputPath: Path, fps: Int = 30): String =
    s"""
    |To create an animated GIF from the generated frames:
    |
    |Using ImageMagick:
    |  magick -delay ${100/fps} -loop 0 ${frameDir}\\frame*.png ${outputPath}
    |
    |Using ffmpeg (better quality):
    |  ffmpeg -framerate $fps -pattern_type glob -i '${frameDir}\\frame*.png' \\
    |    -vf "palettegen" palette.png
    |  ffmpeg -framerate $fps -pattern_type glob -i '${frameDir}\\frame*.png' \\
    |    -i palette.png -lavfi paletteuse ${outputPath}
    |
    |Or on Windows PowerShell:
    |  magick -delay ${100/fps} -loop 0 ${frameDir}\\frame*.png ${outputPath}
    |""".stripMargin

  /** Instructions for creating MP4 video from frames */
  def videoInstructions(frameDir: Path, outputPath: Path, fps: Int = 30): String =
    s"""
    |To create an MP4 video from the generated frames:
    |
    |Using ffmpeg:
    |  ffmpeg -framerate $fps -pattern_type glob -i '${frameDir}\\frame*.png' \\
    |    -c:v libx264 -pix_fmt yuv420p -crf 23 ${outputPath}
    |
    |High quality (larger file):
    |  ffmpeg -framerate $fps -pattern_type glob -i '${frameDir}\\frame*.png' \\
    |    -c:v libx264 -pix_fmt yuv420p -crf 18 -preset slow ${outputPath}
    |
    |On Windows PowerShell:
    |  ffmpeg -framerate $fps -i ${frameDir}\\frame%04d.png \\
    |    -c:v libx264 -pix_fmt yuv420p -crf 23 ${outputPath}
    |""".stripMargin

  /** Print instructions for both GIF and video creation */
  def printInstructions(frameDir: Path, baseName: String = "animation", fps: Int = 30): Unit =
    println()
    println("=" * 80)
    println("Animation Export Instructions")
    println("=" * 80)
    println()
    println("Frames saved to: " + frameDir.toAbsolutePath)
    println()
    println(gifInstructions(frameDir, frameDir.getParent.resolve(s"$baseName.gif"), fps))
    println()
    println(videoInstructions(frameDir, frameDir.getParent.resolve(s"$baseName.mp4"), fps))
    println("=" * 80)

/** Example: Animate NDVI changes over a growing season */
@main def timeSeriesAnimationDemo(): Unit =
  given runtime: VkCyfraRuntime = VkCyfraRuntime()
  
  try
    val outputDir = Paths.get("satellite_output", "animation")
    java.nio.file.Files.createDirectories(outputDir)

    println("Time Series Animation Demo")
    println("=" * 80)
    println()

    // Simulate a time series (in real use, load actual satellite data)
    val dates = (0 until 12).map { month =>
      LocalDateTime.of(2023, month + 1, 15, 12, 0)
    }

    println("Generating synthetic time series...")
    val images = dates.map { date =>
      val seasonProgress = (date.getMonthValue - 1) / 11.0f
      
      // Simulate seasonal vegetation changes
      val image = DataLoader.loadSyntheticData(
        width = 512,  // Smaller for demo
        height = 512,
        includeVegetation = true,
        includeWater = true,
        includeUrban = true
      )
      
      date -> image
    }

    val timeSeries = ImageTimeSeries(images.toList)
    println(s"Created time series with ${images.length} images")
    println()

    // Animate NDVI over time
    val animator = new TimeSeriesAnimator()
    val frames = animator.animateSpectralIndex(
      timeSeries,
      indexName = "NDVI",
      outputDir = outputDir,
      fps = 30,
      totalDuration = 5.seconds
    )

    println()
    println(s"✓ Generated ${frames.length} frames")
    
    // Print export instructions
    AnimationExport.printInstructions(outputDir, "ndvi_timeseries", fps = 30)

  catch
    case e: Exception =>
      println(s"Error: ${e.getMessage}")
      e.printStackTrace()
  finally
    runtime.close()

  println()
  println("Demo complete!")


