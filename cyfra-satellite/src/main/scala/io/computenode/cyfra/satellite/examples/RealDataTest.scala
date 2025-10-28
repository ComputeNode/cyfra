package io.computenode.cyfra.satellite.examples

import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.satellite.spectral.*
import io.computenode.cyfra.satellite.visualization.*
import io.computenode.cyfra.runtime.VkCyfraRuntime
import java.nio.file.Paths
import java.time.LocalDate
import scala.util.{Try, Success, Failure}

@main def testRealData(): Unit =
  println("=" * 80)
  println("Cyfra Satellite Analysis - Real Sentinel-2 Data Test")
  println("=" * 80)
  println()
  
  // Initialize runtime
  given runtime: VkCyfraRuntime = VkCyfraRuntime()
  val analyzer = new SpectralAnalyzer()
  
  try {
    // Test configuration
    val tile = RealDataLoader.TileId.PARIS  // 31UCS - Paris, France
    val date = LocalDate.of(2024, 8, 15)  // August 2024 - more likely to have data
    val outputDir = Paths.get("satellite_data")
    
    println(s"📡 Testing Sentinel-2 Data Loading")
    println(s"   Tile: ${tile.toString}")
    println(s"   Date: $date")
    println()
    
    // Download and load real data
    println(s"⬇️  Downloading Sentinel-2 bands from AWS S3...")
    println(s"   (First run may take 30-60 seconds)")
    println()
    
    val sceneResult = RealDataLoader.loadSentinel2Scene(
      tile = tile,
      date = date,
      bands = List("B02", "B03", "B04", "B08", "B11"),  // Blue, Green, Red, NIR, SWIR
      outputDir = outputDir
    )
    
    sceneResult match {
      case Success(scene) =>
        println(s"✅ Successfully loaded Sentinel-2 scene!")
        println(s"   Dimensions: ${scene.width} × ${scene.height}")
        println(s"   Bands: ${scene.bands.keys.mkString(", ")}")
        println(s"   Satellite: ${scene.metadata.satellite}")
        println(s"   Processing Level: ${scene.metadata.processingLevel}")
        println()
        
        // Create Sentinel-2 standardized band mappings
        val analysisImage = scene.copy(bands = Map(
          "NIR" -> scene.bands("B08"),
          "Red" -> scene.bands("B04"),
          "Green" -> scene.bands("B03"),
          "Blue" -> scene.bands("B02")
        ))
        
        println(s"🎯 Computing spectral indices on GPU...")
        println()
        
        // Compute NDVI
        print("   Computing NDVI... ")
        val startNdvi = System.nanoTime()
        val ndviResult = analyzer.computeNDVI(analysisImage)
        val timeNdvi = (System.nanoTime() - startNdvi) / 1_000_000.0
        println(f"✓ ($timeNdvi%.1f ms)")
        
        // Compute EVI
        print("   Computing EVI... ")
        val startEvi = System.nanoTime()
        val eviResult = analyzer.computeEVI(analysisImage)
        val timeEvi = (System.nanoTime() - startEvi) / 1_000_000.0
        println(f"✓ ($timeEvi%.1f ms)")
        
        // Compute NDWI
        print("   Computing NDWI... ")
        val startNdwi = System.nanoTime()
        val ndwiResult = analyzer.computeNDWI(analysisImage)
        val timeNdwi = (System.nanoTime() - startNdwi) / 1_000_000.0
        println(f"✓ ($timeNdwi%.1f ms)")
        
        println()
        
        // Statistics
        println(s"📊 Statistics:")
        println(f"   NDVI: min=${ndviResult.statistics.min}%7.3f, max=${ndviResult.statistics.max}%7.3f, avg=${ndviResult.statistics.mean}%7.3f")
        println(f"   EVI:  min=${eviResult.statistics.min}%7.3f, max=${eviResult.statistics.max}%7.3f, avg=${eviResult.statistics.mean}%7.3f")
        println(f"   NDWI: min=${ndwiResult.statistics.min}%7.3f, max=${ndwiResult.statistics.max}%7.3f, avg=${ndwiResult.statistics.mean}%7.3f")
        println()
        
        // Export visualizations
        println(s"💾 Exporting visualizations...")
        val outputPath = Paths.get("satellite_output")
        
        ImageExporter.exportAsPNG(
          result = ndviResult,
          outputPath = outputPath.resolve(s"sentinel2_ndvi_${tile.toString}_${date}.png"),
          colorMap = ColorMaps.viridis
        ) match {
          case Success(_) => println(s"   ✓ NDVI: satellite_output/sentinel2_ndvi_${tile.toString}_${date}.png")
          case Failure(ex) => println(s"   ✗ NDVI export failed: ${ex.getMessage}")
        }
        
        ImageExporter.exportAsPNG(
          result = eviResult,
          outputPath = outputPath.resolve(s"sentinel2_evi_${tile.toString}_${date}.png"),
          colorMap = ColorMaps.viridis
        ) match {
          case Success(_) => println(s"   ✓ EVI: satellite_output/sentinel2_evi_${tile.toString}_${date}.png")
          case Failure(ex) => println(s"   ✗ EVI export failed: ${ex.getMessage}")
        }
        
        ImageExporter.exportAsPNG(
          result = ndwiResult,
          outputPath = outputPath.resolve(s"sentinel2_ndwi_${tile.toString}_${date}.png"),
          colorMap = ColorMaps.water
        ) match {
          case Success(_) => println(s"   ✓ NDWI: satellite_output/sentinel2_ndwi_${tile.toString}_${date}.png")
          case Failure(ex) => println(s"   ✗ NDWI export failed: ${ex.getMessage}")
        }
        
        println()
        println("=" * 80)
        println("✅ Real Sentinel-2 Data Test - SUCCESS!")
        println("=" * 80)
        println()
        println(f"Total GPU processing time: ${timeNdvi + timeEvi + timeNdwi}%.1f ms")
        val pixelCount = scene.width * scene.height
        println(f"Image size: ${scene.width}%d × ${scene.height}%d = $pixelCount%,d pixels")
        println()
        println("🎉 Your GPU satellite analysis pipeline is working with real data!")
        
      case Failure(exception) =>
        println(s"❌ Failed to load Sentinel-2 data!")
        println(s"   Error: ${exception.getMessage}")
        println()
        println("Possible causes:")
        println("  - No internet connection")
        println("  - AWS S3 access issues")
        println("  - No data available for this tile/date")
        println("  - File format not supported")
        println()
        exception.printStackTrace()
    }
    
  } catch {
    case e: Exception =>
      println("=" * 80)
      println("❌ FATAL ERROR")
      println("=" * 80)
      e.printStackTrace()
  }

