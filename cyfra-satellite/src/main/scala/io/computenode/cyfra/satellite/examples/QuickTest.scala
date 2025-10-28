package io.computenode.cyfra.satellite.examples

import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.satellite.spectral.*
import io.computenode.cyfra.satellite.visualization.*
import io.computenode.cyfra.runtime.VkCyfraRuntime
import java.nio.file.Paths

/** Quick test with synthetic data to verify the GPU pipeline works */
@main def quickTest(): Unit =
  println("=" * 80)
  println("Cyfra Satellite Analysis - Quick Test (Synthetic Data)")
  println("=" * 80)
  println()
  
  // Initialize runtime
  given runtime: VkCyfraRuntime = VkCyfraRuntime()
  val analyzer = new SpectralAnalyzer()
  
  try {
    // Generate synthetic satellite data
    println("📊 Generating synthetic satellite data...")
    val image = DataLoader.loadSyntheticData(1024, 1024)
    val pixelCount = image.width * image.height
    println(f"   Size: ${image.width}%d × ${image.height}%d = $pixelCount%,d pixels")
    println(s"   Bands: ${image.bands.keys.mkString(", ")}")
    println()
    
    // Compute spectral indices on GPU
    println("🎯 Computing spectral indices on GPU...")
    
    print("   NDVI... ")
    val t1 = System.nanoTime()
    val ndvi = analyzer.computeNDVI(image)
    val time1 = (System.nanoTime() - t1) / 1_000_000.0
    println(f"✓ ($time1%.1f ms)")
    
    print("   EVI...  ")
    val t2 = System.nanoTime()
    val evi = analyzer.computeEVI(image)
    val time2 = (System.nanoTime() - t2) / 1_000_000.0
    println(f"✓ ($time2%.1f ms)")
    
    print("   NDWI... ")
    val t3 = System.nanoTime()
    val ndwi = analyzer.computeNDWI(image)
    val time3 = (System.nanoTime() - t3) / 1_000_000.0
    println(f"✓ ($time3%.1f ms)")
    
    println()
    
    // Statistics
    println("📊 Results:")
    println(f"   NDVI: min=${ndvi.statistics.min}%7.3f, max=${ndvi.statistics.max}%7.3f, mean=${ndvi.statistics.mean}%7.3f")
    println(f"   EVI:  min=${evi.statistics.min}%7.3f, max=${evi.statistics.max}%7.3f, mean=${evi.statistics.mean}%7.3f")
    println(f"   NDWI: min=${ndwi.statistics.min}%7.3f, max=${ndwi.statistics.max}%7.3f, mean=${ndwi.statistics.mean}%7.3f")
    println()
    
    // Export
    println("💾 Exporting visualizations...")
    val outDir = Paths.get("satellite_output")
    ImageExporter.exportAsPNG(ndvi, outDir.resolve("test_ndvi.png"), ColorMaps.viridis)
    ImageExporter.exportAsPNG(evi, outDir.resolve("test_evi.png"), ColorMaps.viridis)
    ImageExporter.exportAsPNG(ndwi, outDir.resolve("test_ndwi.png"), ColorMaps.water)
    println("   ✓ Exported to satellite_output/")
    println()
    
    // Performance summary
    val totalTime = time1 + time2 + time3
    val pixelsPerMs = (image.width * image.height).toDouble / totalTime
    
    println("=" * 80)
    println("✅ SUCCESS - GPU Satellite Analysis Pipeline Working!")
    println("=" * 80)
    println()
    println(f"Total GPU time: $totalTime%.1f ms")
    println(f"Performance: ${pixelsPerMs / 1000}%.1f million pixels/second")
    println()
    println("🎉 Your GPU can process satellite imagery!")
    println()
    println("Next steps:")
    println("  1. Start web server: sbt \"project satellite\" \"runMain io.computenode.cyfra.satellite.web.SatelliteWebServer\"")
    println("  2. Open http://localhost:8080 in your browser")
    println("  3. Use 'Synthetic Data' mode for instant results")
    println("  4. Real Sentinel-2 data requires proper S3 URLs (in progress)")
    
  } catch {
    case e: Exception =>
      println("=" * 80)
      println("❌ ERROR")
      println("=" * 80)
      e.printStackTrace()
  }

