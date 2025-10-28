package io.computenode.cyfra.satellite.examples

import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.satellite.spectral.SpectralAnalyzer
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.runtime.VkCyfraRuntime
import scala.util.Random

/** Quick test of NBR computation using synthetic data.
  * 
  * This example generates synthetic NIR and SWIR bands and computes NBR
  * to demonstrate the GPU pipeline without requiring real satellite data.
  */
object NBRQuickTest:
  
  def main(args: Array[String]): Unit = {
    println("=== NBR Quick Test (Synthetic Data) ===\n")
    
    given VkCyfraRuntime = VkCyfraRuntime()
    
    try {
      // Generate synthetic image with various land cover types
      val image = generateSyntheticImage(512, 512)
      
      println(s"Generated synthetic image: ${image.width}x${image.height} pixels")
      println("Synthetic land covers: healthy vegetation, burned areas, water, bare soil\n")
      
      // Compute NBR on GPU
      val analyzer = SpectralAnalyzer()
      
      val startTime = System.currentTimeMillis()
      val nbrResult = analyzer.computeNBR(image)
      val elapsed = System.currentTimeMillis() - startTime
      
      println(s"✓ NBR computed on GPU in ${elapsed}ms")
      println(s"  Processing rate: ${(image.pixelCount / elapsed.toFloat * 1000).toInt} pixels/second\n")
      
      // Analyze results
      analyzeNBRResults(nbrResult)
      
      println("\n✓ NBR test completed successfully!")
      
    } finally {
      summon[VkCyfraRuntime].close()
    }
  }
  
  /** Generate synthetic satellite image with different land cover types */
  def generateSyntheticImage(width: Int, height: Int): SatelliteImage = {
    val pixelCount = width * height
    val nir = Array.ofDim[Float](pixelCount)
    val swir = Array.ofDim[Float](pixelCount)
    
    val random = new Random(42)
    
    for (y <- 0 until height; x <- 0 until width) {
      val idx = y * width + x
      
      // Create different regions with different land cover types
      val regionX = x / (width / 4)
      val regionY = y / (height / 4)
      val region = regionY * 4 + regionX
      
      // Simulate different land covers in different regions
      val (nirBase, swirBase) = region % 5 match {
        case 0 => // Healthy dense vegetation (high NIR, low SWIR) → high NBR
          (0.5f + random.nextFloat() * 0.2f, 0.1f + random.nextFloat() * 0.1f)
        
        case 1 => // Moderate vegetation
          (0.35f + random.nextFloat() * 0.15f, 0.2f + random.nextFloat() * 0.1f)
        
        case 2 => // Burned area (low NIR, high SWIR) → low/negative NBR
          (0.1f + random.nextFloat() * 0.1f, 0.4f + random.nextFloat() * 0.2f)
        
        case 3 => // Water (low NIR, low SWIR) → negative NBR
          (0.05f + random.nextFloat() * 0.05f, 0.03f + random.nextFloat() * 0.03f)
        
        case _ => // Bare soil/urban (moderate both)
          (0.25f + random.nextFloat() * 0.1f, 0.3f + random.nextFloat() * 0.1f)
      }
      
      // Add some noise
      nir(idx) = nirBase + (random.nextFloat() - 0.5f) * 0.05f
      swir(idx) = swirBase + (random.nextFloat() - 0.5f) * 0.05f
    }
    
    val bands = Map(
      "NIR" -> nir,
      "SWIR" -> swir
    )
    
    val metadata = ImageMetadata(
      satellite = "Synthetic",
      acquisitionTime = java.time.LocalDateTime.now(),
      processingLevel = "Test",
      tileId = None
    )
    
    SatelliteImage(bands, width, height, metadata)
  }
  
  /** Analyze NBR results */
  def analyzeNBRResults(result: SpectralIndexResult): Unit = {
    val values = result.values
    
    // Compute statistics
    val min = values.min
    val max = values.max
    val mean = values.sum / values.length
    val median = {
      val sorted = values.sorted
      sorted(sorted.length / 2)
    }
    
    println("NBR Statistics:")
    println(f"  Min:    ${min}%.3f")
    println(f"  Max:    ${max}%.3f")
    println(f"  Mean:   ${mean}%.3f")
    println(f"  Median: ${median}%.3f")
    
    // Classify pixels
    val healthyVeg = values.count(_ > 0.4f)
    val moderateVeg = values.count(v => v > 0.1f && v <= 0.4f)
    val lowVeg = values.count(v => v > -0.1f && v <= 0.1f)
    val burned = values.count(v => v > -0.4f && v <= -0.1f)
    val water = values.count(_ <= -0.4f)
    
    val total = values.length.toFloat
    
    println("\nLand Cover Classification:")
    println(f"  Healthy Vegetation (NBR > 0.4):     ${healthyVeg * 100 / total}%.1f%%")
    println(f"  Moderate Vegetation (0.1 - 0.4):    ${moderateVeg * 100 / total}%.1f%%")
    println(f"  Sparse/Mixed (-0.1 - 0.1):          ${lowVeg * 100 / total}%.1f%%")
    println(f"  Potential Burn (-0.4 - -0.1):       ${burned * 100 / total}%.1f%%")
    println(f"  Water/Bare (< -0.4):                ${water * 100 / total}%.1f%%")
    
    // Value distribution
    println("\nValue Distribution:")
    val histogram = computeSimpleHistogram(values, 10)
    histogram.zipWithIndex.foreach { case (count, i) =>
      val rangeStart = -1.0f + (i * 0.2f)
      val rangeEnd = rangeStart + 0.2f
      val bar = "█" * (count / 20).toInt
      println(f"  [${rangeStart}%5.2f to ${rangeEnd}%5.2f]: $bar")
    }
  }
  
  /** Simple histogram computation */
  def computeSimpleHistogram(values: Array[Float], bins: Int): Array[Int] = {
    val histogram = Array.ofDim[Int](bins)
    val min = -1.0f
    val max = 1.0f
    val binWidth = (max - min) / bins
    
    values.foreach { v =>
      val clampedV = math.max(min, math.min(max, v))
      val binIndex = math.min(((clampedV - min) / binWidth).toInt, bins - 1)
      histogram(binIndex) += 1
    }
    
    histogram
  }

