package io.computenode.cyfra.satellite.examples

import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.satellite.spectral.SpectralAnalyzer
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.runtime.VkCyfraRuntime
import java.time.LocalDate

/** Test NBR computation with real Sentinel-2 data
  *
  * This downloads NIR (B08) and SWIR (B12) bands and computes NBR.
  * SWIR is at 20m resolution and will be resampled to match NIR at 10m.
  */
object TestNBRReal:
  
  def main(args: Array[String]): Unit = {
    println("=== NBR Test with Real Sentinel-2 Data ===\n")
    
    given VkCyfraRuntime = VkCyfraRuntime()
    
    try {
      // Test with Paris tile - recent data
      val tile = RealDataLoader.TileId.PARIS
      val date = LocalDate.of(2024, 10, 21)
      
      println(s"Loading Sentinel-2 scene:")
      println(s"  Tile: ${tile} (Paris, France)")
      println(s"  Date: ${date}")
      println(s"  Bands: B08 (NIR, 10m), B12 (SWIR, 20m)")
      println()
      
      // Load scene with NIR and SWIR bands
      val result = RealDataLoader.loadSentinel2Scene(
        tile = tile,
        date = date,
        bands = List("B08", "B12"),  // NIR at 10m, SWIR at 20m
        targetSize = 1024  // Limit to 1024x1024 for speed
      )
      
      result match {
        case scala.util.Success(image) =>
          println(s"\n✓ Loaded image: ${image.width}x${image.height} pixels")
          println(s"  Total pixels: ${image.pixelCount}")
          println(s"  Bands available: ${image.bands.keys.mkString(", ")}")
          
          // Verify we have the required bands
          if (!image.hasBand("NIR")) {
            println("✗ ERROR: NIR band not found!")
            return
          }
          if (!image.hasBand("SWIR")) {
            println("✗ ERROR: SWIR band not found!")
            return
          }
          
          println(s"\n--- Computing NBR on GPU ---")
          
          // Compute NBR
          val analyzer = SpectralAnalyzer()
          val startTime = System.currentTimeMillis()
          val nbrResult = analyzer.computeNBR(image)
          val elapsed = System.currentTimeMillis() - startTime
          
          println(s"✓ NBR computed in ${elapsed}ms")
          println(f"  Processing rate: ${image.pixelCount.toFloat / elapsed * 1000}%.0f pixels/second")
          
          // Analyze results
          println(s"\n--- NBR Analysis ---")
          analyzeNBR(nbrResult)
          
          println("\n✓ Test completed successfully!")
          
        case scala.util.Failure(e) =>
          println(s"\n✗ Failed to load Sentinel-2 data:")
          println(s"   ${e.getMessage}")
          println()
          
          if (e.getMessage.contains("Authentication Required")) {
            println("Setup instructions:")
            println("1. Register at https://dataspace.copernicus.eu/")
            println("2. Get credentials from your account settings")
            println("3. Set environment variables:")
            println("   - COPERNICUS_CLIENT_ID")
            println("   - COPERNICUS_CLIENT_SECRET")
            println()
            println("Alternatively, run the synthetic test:")
            println("  sbt \"project satellite\" \"runMain io.computenode.cyfra.satellite.examples.NBRQuickTest\"")
          } else {
            println("\nTrying alternative date/tile...")
            tryAlternative()
          }
      }
      
    } finally {
      summon[VkCyfraRuntime].close()
    }
  }
  
  /** Try alternative tile/date if first attempt fails */
  def tryAlternative()(using VkCyfraRuntime): Unit = {
    println("\n--- Trying London tile instead ---")
    
    val result = RealDataLoader.loadSentinel2Scene(
      tile = RealDataLoader.TileId.LONDON,
      date = LocalDate.of(2024, 10, 25),
      bands = List("B08", "B12"),
      targetSize = 1024
    )
    
    result match {
      case scala.util.Success(image) =>
        println(s"✓ Alternative tile loaded: ${image.width}x${image.height} pixels")
        
        val analyzer = SpectralAnalyzer()
        val nbrResult = analyzer.computeNBR(image)
        
        analyzeNBR(nbrResult)
        println("\n✓ Alternative test succeeded!")
        
      case scala.util.Failure(e) =>
        println(s"✗ Alternative also failed: ${e.getMessage}")
        println("\nPlease run the synthetic test instead:")
        println("  sbt \"project satellite\" \"runMain io.computenode.cyfra.satellite.examples.NBRQuickTest\"")
    }
  }
  
  /** Analyze NBR results */
  def analyzeNBR(result: SpectralIndexResult): Unit = {
    val values = result.values
    
    // Statistics
    val min = values.min
    val max = values.max
    val mean = values.sum / values.length
    val sorted = values.sorted
    val median = sorted(sorted.length / 2)
    val p10 = sorted((sorted.length * 0.1).toInt)
    val p90 = sorted((sorted.length * 0.9).toInt)
    
    println("\nNBR Statistics:")
    println(f"  Min:    ${min}%7.3f")
    println(f"  P10:    ${p10}%7.3f")
    println(f"  Median: ${median}%7.3f")
    println(f"  Mean:   ${mean}%7.3f")
    println(f"  P90:    ${p90}%7.3f")
    println(f"  Max:    ${max}%7.3f")
    
    // Land cover classification based on NBR values
    val healthyVeg = values.count(_ > 0.4f)
    val moderateVeg = values.count(v => v > 0.1f && v <= 0.4f)
    val sparse = values.count(v => v > -0.1f && v <= 0.1f)
    val burned = values.count(v => v > -0.4f && v <= -0.1f)
    val waterBare = values.count(_ <= -0.4f)
    
    val total = values.length.toFloat
    
    println("\nLand Cover Classification:")
    println(f"  Healthy Vegetation (> 0.4):       ${healthyVeg * 100 / total}%6.2f%% (${healthyVeg}%8d pixels)")
    println(f"  Moderate Vegetation (0.1-0.4):    ${moderateVeg * 100 / total}%6.2f%% (${moderateVeg}%8d pixels)")
    println(f"  Sparse/Mixed (-0.1-0.1):          ${sparse * 100 / total}%6.2f%% (${sparse}%8d pixels)")
    println(f"  Potential Burn/Stress (-0.4--0.1): ${burned * 100 / total}%6.2f%% (${burned}%8d pixels)")
    println(f"  Water/Bare (< -0.4):              ${waterBare * 100 / total}%6.2f%% (${waterBare}%8d pixels)")
    
    // Visual histogram
    println("\nNBR Distribution:")
    val histogram = computeHistogram(values, -1.0f, 1.0f, 15)
    val maxCount = histogram.max
    
    histogram.zipWithIndex.foreach { case (count, i) =>
      val rangeStart = -1.0f + (i * 2.0f / 15)
      val rangeEnd = rangeStart + (2.0f / 15)
      val barLength = if (maxCount > 0) (count.toFloat / maxCount * 40).toInt else 0
      val bar = "█" * barLength
      println(f"  ${rangeStart}%5.2f to ${rangeEnd}%5.2f: $bar%-40s ${count}%7d")
    }
    
    // Potential anomalies
    val veryLowNBR = values.count(_ < -0.5f)
    if (veryLowNBR > total * 0.01f) {
      println(f"\n⚠️  Detected ${veryLowNBR * 100 / total}%.2f%% pixels with very low NBR (< -0.5)")
      println("   This could indicate water bodies, urban areas, or severely degraded land")
    }
    
    val potentialBurn = values.count(v => v > -0.3f && v < 0.0f)
    if (potentialBurn > total * 0.05f) {
      println(f"\n⚠️  Detected ${potentialBurn * 100 / total}%.2f%% pixels with NBR suggesting stress/burn")
      println("   Consider comparing with historical data for change detection")
    }
  }
  
  /** Compute histogram */
  def computeHistogram(values: Array[Float], min: Float, max: Float, bins: Int): Array[Int] = {
    val histogram = Array.ofDim[Int](bins)
    val binWidth = (max - min) / bins
    
    values.foreach { v =>
      val clampedV = math.max(min, math.min(max - 0.0001f, v))
      val binIndex = ((clampedV - min) / binWidth).toInt
      if (binIndex >= 0 && binIndex < bins) {
        histogram(binIndex) += 1
      }
    }
    
    histogram
  }

