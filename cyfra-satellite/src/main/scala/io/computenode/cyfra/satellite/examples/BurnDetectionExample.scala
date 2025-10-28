package io.computenode.cyfra.satellite.examples

import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.satellite.spectral.{SpectralAnalyzer, SpectralPrograms}
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.runtime.VkCyfraRuntime
import java.time.LocalDate

/** Example demonstrating NBR (Normalized Burn Ratio) for fire and burn detection.
  *
  * NBR is particularly useful for:
  * - Post-fire damage assessment
  * - Burn severity mapping
  * - Active fire monitoring
  * - Recovery tracking over time
  *
  * The dNBR (difference NBR) between pre and post-fire images provides burn severity:
  * - Unburned: dNBR < 0.1
  * - Low severity: 0.1 to 0.27
  * - Moderate-low: 0.27 to 0.44
  * - Moderate-high: 0.44 to 0.66
  * - High severity: > 0.66
  */
object BurnDetectionExample:
  
  def main(args: Array[String]): Unit = {
    println("=== NBR Burn Detection Example ===\n")
    
    given VkCyfraRuntime = VkCyfraRuntime()
    
    try {
      // Example 1: Single image NBR calculation
      exampleSingleImageNBR()
      
      // Example 2: Temporal change detection (dNBR)
      exampleBurnSeverityMapping()
      
      // Example 3: Statistics and classification
      exampleBurnStatistics()
      
    } finally {
      summon[VkCyfraRuntime].close()
    }
  }
  
  /** Example 1: Compute NBR for a single image */
  def exampleSingleImageNBR()(using runtime: VkCyfraRuntime): Unit = {
    println("\n--- Example 1: Single Image NBR ---")
    
    // Try loading real Sentinel-2 data with SWIR band
    val result = RealDataLoader.loadSentinel2Scene(
      tile = RealDataLoader.TileId.PARIS,
      date = LocalDate.of(2024, 10, 15),
      bands = List("B08", "B12")  // NIR and SWIR for NBR
    )
    
    result match {
      case scala.util.Success(image) =>
        println(s"✓ Loaded image: ${image.width}x${image.height} pixels")
        
        // Compute NBR on GPU
        val analyzer = SpectralAnalyzer()
        val startTime = System.currentTimeMillis()
        val nbrResult = analyzer.computeNBR(image)
        val elapsed = System.currentTimeMillis() - startTime
        
        println(s"✓ NBR computed in ${elapsed}ms")
        
        // Analyze results
        val stats = computeStats(nbrResult.values)
        println(s"\nNBR Statistics:")
        println(f"  Min: ${stats.min}%.3f")
        println(f"  Max: ${stats.max}%.3f")
        println(f"  Mean: ${stats.mean}%.3f")
        println(f"  Std: ${stats.stdDev}%.3f")
        
        // Classify vegetation health
        val healthyVeg = nbrResult.values.count(_ > 0.4f)
        val moderateBurn = nbrResult.values.count(v => v > 0.1f && v <= 0.4f)
        val severeBurn = nbrResult.values.count(v => v > -0.4f && v <= 0.1f)
        val water = nbrResult.values.count(_ <= -0.4f)
        
        println(s"\nLand Cover Classification:")
        println(f"  Healthy Vegetation: ${healthyVeg * 100.0 / nbrResult.values.length}%.1f%%")
        println(f"  Moderate Burn: ${moderateBurn * 100.0 / nbrResult.values.length}%.1f%%")
        println(f"  Severe Burn: ${severeBurn * 100.0 / nbrResult.values.length}%.1f%%")
        println(f"  Water/Bare: ${water * 100.0 / nbrResult.values.length}%.1f%%")
        
      case scala.util.Failure(e) =>
        println(s"⚠️  Could not load real data: ${e.getMessage}")
        println("   This example requires Sentinel-2 data with SWIR bands (B12)")
    }
  }
  
  /** Example 2: Burn severity mapping using dNBR (difference NBR) */
  def exampleBurnSeverityMapping()(using runtime: VkCyfraRuntime): Unit = {
    println("\n--- Example 2: Burn Severity Mapping (dNBR) ---")
    
    // Load pre-fire image
    val preFireResult = RealDataLoader.loadSentinel2Scene(
      tile = RealDataLoader.TileId.PARIS,
      date = LocalDate.of(2024, 6, 1),  // Before fire
      bands = List("B08", "B12")
    )
    
    // Load post-fire image
    val postFireResult = RealDataLoader.loadSentinel2Scene(
      tile = RealDataLoader.TileId.PARIS,
      date = LocalDate.of(2024, 10, 15),  // After fire
      bands = List("B08", "B12")
    )
    
    (preFireResult, postFireResult) match {
      case (scala.util.Success(preFire), scala.util.Success(postFire)) =>
        val analyzer = SpectralAnalyzer()
        
        // Compute NBR for both images
        val preNBR = analyzer.computeNBR(preFire)
        val postNBR = analyzer.computeNBR(postFire)
        
        // Calculate dNBR (difference)
        val dNBR = preNBR.values.zip(postNBR.values).map { case (pre, post) =>
          pre - post  // High positive values indicate severe burn
        }
        
        // Classify burn severity
        val unburned = dNBR.count(_ < 0.1f)
        val lowSeverity = dNBR.count(v => v >= 0.1f && v < 0.27f)
        val moderateLow = dNBR.count(v => v >= 0.27f && v < 0.44f)
        val moderateHigh = dNBR.count(v => v >= 0.44f && v < 0.66f)
        val highSeverity = dNBR.count(v => v >= 0.66f)
        
        val total = dNBR.length.toFloat
        
        println("\nBurn Severity Classification (dNBR):")
        println(f"  Unburned (< 0.1): ${unburned * 100.0 / total}%.1f%%")
        println(f"  Low Severity (0.1-0.27): ${lowSeverity * 100.0 / total}%.1f%%")
        println(f"  Moderate-Low (0.27-0.44): ${moderateLow * 100.0 / total}%.1f%%")
        println(f"  Moderate-High (0.44-0.66): ${moderateHigh * 100.0 / total}%.1f%%")
        println(f"  High Severity (> 0.66): ${highSeverity * 100.0 / total}%.1f%%")
        
        // Calculate total burned area
        val burnedPixels = dNBR.count(_ >= 0.1f)
        val pixelAreaHa = 0.01  // 10m x 10m = 100m² = 0.01 hectares
        val burnedAreaHa = burnedPixels * pixelAreaHa
        
        println(f"\nTotal Burned Area: ${burnedAreaHa}%.1f hectares (${burnedAreaHa / 100}%.1f km²)")
        
      case _ =>
        println("⚠️  Could not load temporal data for burn severity analysis")
        println("   This example requires pre and post-fire Sentinel-2 images")
    }
  }
  
  /** Example 3: Detailed burn statistics */
  def exampleBurnStatistics()(using runtime: VkCyfraRuntime): Unit = {
    println("\n--- Example 3: Burn Statistics ---")
    
    val result = RealDataLoader.loadSentinel2Scene(
      tile = RealDataLoader.TileId.PARIS,
      date = LocalDate.of(2024, 10, 15),
      bands = List("B08", "B12")
    )
    
    result match {
      case scala.util.Success(image) =>
        val analyzer = SpectralAnalyzer()
        val nbrResult = analyzer.computeNBR(image)
        
        // Compute histogram
        val bins = 20
        val histogram = computeHistogram(nbrResult.values, -1.0f, 1.0f, bins)
        
        println("\nNBR Distribution Histogram:")
        histogram.zipWithIndex.foreach { case (count, i) =>
          val binStart = -1.0f + (i * 2.0f / bins)
          val binEnd = binStart + (2.0f / bins)
          val bar = "#" * (count / (nbrResult.values.length / 100))
          println(f"  [${binStart}%.2f to ${binEnd}%.2f]: $bar ($count pixels)")
        }
        
        // Detect potential fire-affected areas (low NBR values)
        val potentialBurnPixels = nbrResult.values.count(v => v < 0.2f && v > -0.3f)
        println(f"\nPotential burn-affected areas: ${potentialBurnPixels * 100.0 / nbrResult.values.length}%.1f%% of image")
        
      case scala.util.Failure(e) =>
        println(s"⚠️  Could not load data: ${e.getMessage}")
    }
  }
  
  /** Compute basic statistics */
  case class Statistics(min: Float, max: Float, mean: Float, stdDev: Float)
  
  def computeStats(values: Array[Float]): Statistics = {
    val min = values.min
    val max = values.max
    val mean = values.sum / values.length
    val variance = values.map(v => math.pow(v - mean, 2)).sum / values.length
    val stdDev = math.sqrt(variance).toFloat
    Statistics(min, max, mean, stdDev)
  }
  
  /** Compute histogram */
  def computeHistogram(values: Array[Float], min: Float, max: Float, bins: Int): Array[Int] = {
    val histogram = Array.ofDim[Int](bins)
    val binWidth = (max - min) / bins
    
    values.foreach { v =>
      if (v >= min && v <= max) {
        val binIndex = math.min(((v - min) / binWidth).toInt, bins - 1)
        histogram(binIndex) += 1
      }
    }
    
    histogram
  }

