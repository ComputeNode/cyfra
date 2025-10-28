package io.computenode.cyfra.satellite.examples

import io.computenode.cyfra.satellite.data.RealDataLoader
import java.time.LocalDate
import java.nio.file.Paths

@main def testRealDownload(): Unit = {
  println("=" * 80)
  println("Testing Real Data Download with Detailed Error Messages")
  println("=" * 80)
  
  val tile = RealDataLoader.TileId.parse("31UCS").get
  val date = LocalDate.of(2025, 10, 24)
  val outputDir = Paths.get("satellite_data_test")
  
  println(s"\nAttempting to load: Tile $tile on $date")
  println(s"Output directory: $outputDir")
  println()
  
  RealDataLoader.loadSentinel2Scene(tile, date, outputDir = outputDir) match {
    case scala.util.Success(image) =>
      println(s"\n✅ SUCCESS!")
      println(s"   Width: ${image.width}")
      println(s"   Height: ${image.height}")
      println(s"   Bands: ${image.bands.keys.mkString(", ")}")
      
    case scala.util.Failure(ex) =>
      println(s"\n❌ FAILED!")
      println(s"   Error: ${ex.getMessage}")
      println(s"\n   Full stack trace:")
      ex.printStackTrace()
  }
}

