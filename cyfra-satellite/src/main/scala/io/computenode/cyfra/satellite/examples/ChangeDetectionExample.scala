package io.computenode.cyfra.satellite.examples

import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.satellite.change.*
import io.computenode.cyfra.satellite.spectral.SpectralAnalyzer
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.runtime.VkCyfraRuntime
import java.time.LocalDate
import java.nio.file.Paths

/** Example demonstrating sharp change detection for logging and fires
  * 
  * This uses multi-temporal analysis to detect:
  * - Wildfire burn scars
  * - Logging/deforestation
  * - Land clearing
  * - Post-disaster changes
  */
object ChangeDetectionExample:
  
  def main(args: Array[String]): Unit = {
    println("=" * 80)
    println("SHARP CHANGE DETECTION: Logging & Fire Detection")
    println("=" * 80)
    println()
    
    given VkCyfraRuntime = VkCyfraRuntime()
    
    try {
      // Example 1: Fire detection (California)
      println("\n" + "=" * 80)
      println("EXAMPLE 1: Wildfire Detection")
      println("=" * 80)
      exampleFireDetection()
      
      // Example 2: Logging detection (Amazon)
      println("\n" + "=" * 80)
      println("EXAMPLE 2: Logging/Deforestation Detection")
      println("=" * 80)
      exampleLoggingDetection()
      
      // Example 3: Comprehensive change analysis
      println("\n" + "=" * 80)
      println("EXAMPLE 3: Comprehensive Change Analysis")
      println("=" * 80)
      exampleComprehensiveAnalysis()
      
    } finally {
      summon[VkCyfraRuntime].close()
    }
  }
  
  /** Example 1: Detect wildfire burn areas */
  def exampleFireDetection()(using VkCyfraRuntime): Unit = {
    println("\nDetecting wildfire burn scars using dNBR (Difference NBR)")
    println("Location: California fire zones")
    println("Method: USGS standard burn severity mapping\n")
    
    // Try to load pre and post-fire imagery
    val beforeDate = LocalDate.of(2024, 6, 1)  // Before fire season
    val afterDate = LocalDate.of(2024, 10, 15) // After fire season
    
    val result = for {
      before <- RealDataLoader.loadSentinel2Scene(
        tile = RealDataLoader.TileId("10SEG"), // San Francisco Bay Area
        date = beforeDate,
        bands = List("B08", "B12"), // NIR and SWIR for NBR
        targetSize = 1024
      )
      after <- RealDataLoader.loadSentinel2Scene(
        tile = RealDataLoader.TileId("10SEG"),
        date = afterDate,
        bands = List("B08", "B12"),
        targetSize = 1024
      )
    } yield {
      println(s"✓ Loaded imagery: ${before.width}x${before.height} pixels")
      println(s"  Before: $beforeDate")
      println(s"  After:  $afterDate")
      println(s"  Time window: ~4 months")
      println()
      
      // Compute NBR for both images
      val analyzer = SpectralAnalyzer()
      
      println("Computing NBR values...")
      val nbrBefore = analyzer.computeNBR(before)
      val nbrAfter = analyzer.computeNBR(after)
      
      // Compute dNBR on GPU
      val changeAnalyzer = ChangeAnalyzer()
      val startTime = System.currentTimeMillis()
      val (dnbr, severity) = changeAnalyzer.computeDNBR(
        nbrBefore.values,
        nbrAfter.values,
        before.width,
        before.height
      )
      val elapsed = System.currentTimeMillis() - startTime
      
      println(s"✓ dNBR computed in ${elapsed}ms on GPU")
      println()
      
      // Analyze burn severity
      analyzeBurnSeverity(dnbr, severity)
    }
    
    result match {
      case scala.util.Success(_) => // Already printed inside
      case scala.util.Failure(e) =>
        println(s"⚠️  Could not load real data: ${e.getMessage}")
        println("   Using synthetic example instead...")
        syntheticFireExample()
    }
  }
  
  /** Example 2: Detect logging/deforestation */
  def exampleLoggingDetection()(using VkCyfraRuntime): Unit = {
    println("\nDetecting logging and deforestation using NDVI drops")
    println("Location: Amazon rainforest")
    println("Method: Rapid vegetation loss detection\n")
    
    // Try to load Amazon rainforest imagery
    val beforeDate = LocalDate.of(2024, 1, 15)
    val afterDate = LocalDate.of(2024, 10, 15)
    
    val result = for {
      before <- RealDataLoader.loadSentinel2Scene(
        tile = RealDataLoader.TileId("21LXF"), // Amazon
        date = beforeDate,
        bands = List("B04", "B08", "B12"), // Red, NIR, SWIR
        targetSize = 1024
      )
      after <- RealDataLoader.loadSentinel2Scene(
        tile = RealDataLoader.TileId("21LXF"),
        date = afterDate,
        bands = List("B04", "B08", "B12"),
        targetSize = 1024
      )
    } yield {
      println(s"✓ Loaded imagery: ${before.width}x${before.height} pixels")
      println(s"  Before: $beforeDate")
      println(s"  After:  $afterDate")
      println(s"  Time window: ~9 months")
      println()
      
      // Full change detection
      val changeAnalyzer = ChangeAnalyzer()
      
      println("Detecting changes...")
      val startTime = System.currentTimeMillis()
      val changes = changeAnalyzer.detectChanges(before, after)
      val elapsed = System.currentTimeMillis() - startTime
      
      println(s"✓ Change detection completed in ${elapsed}ms on GPU")
      println()
      
      // Analyze results
      analyzeDeforestation(changes)
    }
    
    result match {
      case scala.util.Success(_) => // Already printed inside
      case scala.util.Failure(e) =>
        println(s"⚠️  Could not load real data: ${e.getMessage}")
        println("   Using synthetic example instead...")
        syntheticLoggingExample()
    }
  }
  
  /** Example 3: Comprehensive analysis (fires + logging) */
  def exampleComprehensiveAnalysis()(using VkCyfraRuntime): Unit = {
    println("\nComprehensive change analysis")
    println("Detecting all types of disturbances simultaneously\n")
    
    // Generate synthetic multi-temporal data
    println("Generating synthetic imagery with simulated changes...")
    val (before, after) = generateSyntheticChangeScenario()
    
    println(s"✓ Generated: ${before.width}x${before.height} pixels")
    println("  Simulated changes:")
    println("    - Fire scar (top-left)")
    println("    - Logging area (top-right)")
    println("    - Regrowth (bottom-left)")
    println("    - Stable (bottom-right)")
    println()
    
    // Run comprehensive detection
    val changeAnalyzer = ChangeAnalyzer()
    
    println("Running comprehensive change detection on GPU...")
    val startTime = System.currentTimeMillis()
    val changes = changeAnalyzer.detectChanges(before, after)
    val elapsed = System.currentTimeMillis() - startTime
    
    println(s"✓ Analysis completed in ${elapsed}ms")
    println(f"  Processing rate: ${changes.pixelCount.toFloat / elapsed * 1000}%.0f pixels/second")
    println()
    
    // Report results
    reportComprehensiveResults(changes)
  }
  
  /** Analyze burn severity results */
  def analyzeBurnSeverity(dnbr: Array[Float], severity: Array[Int]): Unit = {
    println("Burn Severity Analysis (USGS Classification):")
    println("-" * 60)
    
    val severityNames = Map(
      0 -> "Enhanced Regrowth",
      1 -> "Unburned",
      2 -> "Low Severity",
      3 -> "Moderate-Low Severity",
      4 -> "Moderate-High Severity",
      5 -> "High Severity"
    )
    
    val counts = severity.groupBy(identity).view.mapValues(_.length).toMap
    val total = severity.length.toFloat
    
    severityNames.toSeq.sortBy(_._1).foreach { case (code, name) =>
      val count = counts.getOrElse(code, 0)
      val percent = count * 100.0 / total
      val bar = "█" * (percent / 2).toInt
      println(f"  $name%-25s: $percent%6.2f%% $bar")
    }
    println()
    
    // Calculate burned area
    val burnedPixels = severity.count(_ >= 2) // Low severity and above
    val burnedPercent = burnedPixels * 100.0 / total
    val pixelAreaHa = 0.01 // 10m x 10m = 100m² = 0.01 hectares
    val burnedAreaHa = burnedPixels * pixelAreaHa
    
    println(f"Total Burned Area: ${burnedPercent}%.2f%% ($burnedPixels pixels)")
    println(f"  Estimated: ${burnedAreaHa}%.1f hectares (${burnedAreaHa / 100}%.2f km²)")
    
    // Find high severity areas
    val highSeverity = severity.count(_ >= 4)
    if (highSeverity > total * 0.01) {
      println(f"\n⚠️  WARNING: ${highSeverity * 100.0 / total}%.2f%% high severity burn detected")
      println("  Recommend field verification and erosion mitigation")
    }
  }
  
  /** Analyze deforestation results */
  def analyzeDeforestation(changes: ChangeResult): Unit = {
    println("Deforestation Analysis:")
    println("-" * 60)
    
    val typeCounts = changes.changeTypeCounts
    val total = changes.pixelCount.toFloat
    
    typeCounts.foreach { case (name, count) =>
      val percent = count * 100.0 / total
      val bar = "█" * (percent / 2).toInt
      println(f"  $name%-20s: $percent%6.2f%% ($count%7d pixels) $bar")
    }
    println()
    
    // Deforestation metrics
    val loggingPixels = typeCounts.getOrElse("Logging", 0)
    val firePixels = typeCounts.getOrElse("Fire", 0)
    val disturbedPixels = loggingPixels + firePixels
    
    val pixelAreaHa = 0.01
    val deforestedHa = loggingPixels * pixelAreaHa
    val burnedHa = firePixels * pixelAreaHa
    
    println("Disturbance Summary:")
    println(f"  Logging/Clearing:  ${deforestedHa}%8.1f ha (${deforestedHa / 100}%.2f km²)")
    println(f"  Fire Damage:       ${burnedHa}%8.1f ha (${burnedHa / 100}%.2f km²)")
    println(f"  Total Disturbed:   ${(deforestedHa + burnedHa)}%8.1f ha (${(deforestedHa + burnedHa) / 100}%.2f km²)")
    
    // Disturbance score analysis
    val (minDist, maxDist, avgDist) = changes.disturbanceStats
    println()
    println(f"Disturbance Score Statistics:")
    println(f"  Min: ${minDist}%.3f, Max: ${maxDist}%.3f, Avg: ${avgDist}%.3f")
    
    // High confidence detections
    val highConfidence = changes.disturbanceScore.count(_ > 0.7f)
    if (highConfidence > 0) {
      println(f"\n✓ Found $highConfidence high-confidence disturbance pixels")
      println("  Recommend priority for field verification")
    }
  }
  
  /** Report comprehensive results */
  def reportComprehensiveResults(changes: ChangeResult): Unit = {
    println("Change Detection Results:")
    println("=" * 60)
    
    val typeCounts = changes.changeTypeCounts
    val total = changes.pixelCount.toFloat
    
    println("\nChange Type Distribution:")
    typeCounts.foreach { case (name, count) =>
      val percent = count * 100.0 / total
      println(f"  $name%-20s: $percent%6.2f%% ($count pixels)")
    }
    
    println("\nChange Magnitude Statistics:")
    val ndviChanges = changes.ndviChange.filter(_ > 0.1f)
    val nbrChanges = changes.nbrChange.filter(_ > 0.1f)
    
    if (ndviChanges.nonEmpty) {
      println(f"  NDVI drops > 0.1: ${ndviChanges.length} pixels")
      println(f"    Mean drop: ${ndviChanges.sum / ndviChanges.length}%.3f")
      println(f"    Max drop:  ${ndviChanges.max}%.3f")
    }
    
    if (nbrChanges.nonEmpty) {
      println(f"  NBR drops > 0.1: ${nbrChanges.length} pixels")
      println(f"    Mean drop: ${nbrChanges.sum / nbrChanges.length}%.3f")
      println(f"    Max drop:  ${nbrChanges.max}%.3f")
    }
  }
  
  /** Generate synthetic change scenario for testing */
  def generateSyntheticChangeScenario(): (SatelliteImage, SatelliteImage) = {
    val width = 512
    val height = 512
    val pixelCount = width * height
    
    val nirBefore = Array.ofDim[Float](pixelCount)
    val redBefore = Array.ofDim[Float](pixelCount)
    val swirBefore = Array.ofDim[Float](pixelCount)
    
    val nirAfter = Array.ofDim[Float](pixelCount)
    val redAfter = Array.ofDim[Float](pixelCount)
    val swirAfter = Array.ofDim[Float](pixelCount)
    
    val random = new scala.util.Random(42)
    
    for (y <- 0 until height; x <- 0 until width) {
      val idx = y * width + x
      val quadX = x / (width / 2)
      val quadY = y / (height / 2)
      val quad = quadY * 2 + quadX
      
      // Before: All healthy forest
      nirBefore(idx) = 0.5f + random.nextFloat() * 0.1f
      redBefore(idx) = 0.15f + random.nextFloat() * 0.05f
      swirBefore(idx) = 0.1f + random.nextFloat() * 0.05f
      
      // After: Different changes per quadrant
      quad match {
        case 0 => // Top-left: Fire
          nirAfter(idx) = 0.15f + random.nextFloat() * 0.1f
          redAfter(idx) = 0.2f + random.nextFloat() * 0.1f
          swirAfter(idx) = 0.35f + random.nextFloat() * 0.15f
        
        case 1 => // Top-right: Logging
          nirAfter(idx) = 0.2f + random.nextFloat() * 0.1f
          redAfter(idx) = 0.25f + random.nextFloat() * 0.1f
          swirAfter(idx) = 0.2f + random.nextFloat() * 0.1f
        
        case 2 => // Bottom-left: Regrowth
          nirAfter(idx) = 0.6f + random.nextFloat() * 0.1f
          redAfter(idx) = 0.12f + random.nextFloat() * 0.05f
          swirAfter(idx) = 0.08f + random.nextFloat() * 0.05f
        
        case _ => // Bottom-right: Stable
          nirAfter(idx) = nirBefore(idx) + (random.nextFloat() - 0.5f) * 0.02f
          redAfter(idx) = redBefore(idx) + (random.nextFloat() - 0.5f) * 0.01f
          swirAfter(idx) = swirBefore(idx) + (random.nextFloat() - 0.5f) * 0.01f
      }
    }
    
    val metadata = ImageMetadata(
      satellite = "Synthetic",
      acquisitionTime = java.time.LocalDateTime.now(),
      processingLevel = "Test",
      tileId = None
    )
    
    val before = SatelliteImage(
      Map("NIR" -> nirBefore, "Red" -> redBefore, "SWIR" -> swirBefore),
      width, height, metadata
    )
    
    val after = SatelliteImage(
      Map("NIR" -> nirAfter, "Red" -> redAfter, "SWIR" -> swirAfter),
      width, height, metadata
    )
    
    (before, after)
  }
  
  /** Synthetic fire detection example */
  def syntheticFireExample()(using VkCyfraRuntime): Unit = {
    println("Running synthetic fire detection example...")
    val (before, after) = generateSyntheticChangeScenario()
    
    val analyzer = SpectralAnalyzer()
    val nbrBefore = analyzer.computeNBR(before)
    val nbrAfter = analyzer.computeNBR(after)
    
    val changeAnalyzer = ChangeAnalyzer()
    val (dnbr, severity) = changeAnalyzer.computeDNBR(
      nbrBefore.values,
      nbrAfter.values,
      before.width,
      before.height
    )
    
    analyzeBurnSeverity(dnbr, severity)
  }
  
  /** Synthetic logging detection example */
  def syntheticLoggingExample()(using VkCyfraRuntime): Unit = {
    println("Running synthetic logging detection example...")
    val (before, after) = generateSyntheticChangeScenario()
    
    val changeAnalyzer = ChangeAnalyzer()
    val changes = changeAnalyzer.detectChanges(before, after)
    
    analyzeDeforestation(changes)
  }

