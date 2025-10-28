package io.computenode.cyfra.satellite.data

import scala.util.{Try, Success, Failure}
import java.nio.file.{Path, Paths, Files}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.LocalDate
import javax.imageio.{ImageIO, ImageReader}
import javax.imageio.stream.ImageInputStream
import java.awt.image.{BufferedImage, Raster}
import scala.jdk.CollectionConverters.*

/** Loads real Sentinel-2 data from AWS S3 public dataset
  *
  */
object RealDataLoader:
  
  // Copernicus Data Space Ecosystem - Official ESA data source
  // Free access, registration at dataspace.copernicus.eu
  private val COPERNICUS_STAC_API = "https://catalogue.dataspace.copernicus.eu/stac"
  private val COPERNICUS_ODATA_API = "https://catalogue.dataspace.copernicus.eu/odata/v1"
  private val COPERNICUS_DOWNLOAD_API = "https://download.dataspace.copernicus.eu/odata/v1"
  
  private val httpClient = HttpClient.newBuilder()
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()
  
  // Cache directory for downloaded products
  private val CACHE_DIR = Paths.get("satellite_cache")
  
  // Initialize cache directory
  Files.createDirectories(CACHE_DIR)
  
  /** Sentinel-2 tile identifier
    * Example: "31UCS" = UTM zone 31, latitude band U, grid square CS
    */
  case class TileId(utmZone: Int, latBand: String, gridSquare: String):
    override def toString: String = s"$utmZone$latBand$gridSquare"
    
    def toPath: String = s"$utmZone/$latBand/$gridSquare"
  
  object TileId:
    // Popular test tiles
    val PARIS = TileId(31, "U", "CS")
    val LONDON = TileId(30, "U", "WU") 
    val NEW_YORK = TileId(18, "T", "WL")
    val AMAZON = TileId(21, "L", "XF")
    val SAHARA = TileId(32, "R", "MQ")
    
    def parse(s: String): Try[TileId] = Try {
      val utmZone = s.take(2).toInt
      val latBand = s.charAt(2).toString
      val gridSquare = s.drop(3)
      TileId(utmZone, latBand, gridSquare)
    }
  
  /** Download a specific band from Sentinel-2 L2A using Copernicus Data Space
    *
    * Uses OAuth authentication + OData API
    *
    * Set environment variables for authentication:
    * - COPERNICUS_CLIENT_ID
    * - COPERNICUS_CLIENT_SECRET
    *
    * Register at: https://dataspace.copernicus.eu/
    *
    * @param tile Tile identifier (e.g., "31UCS" for Paris)
    * @param date Date of acquisition
    * @param band Band identifier (e.g., "B04" for Red, "B08" for NIR)
    * @param resolution Resolution in meters (10, 20, or 60)
    * @param outputDir Directory to save the downloaded file
    * @return Path to the downloaded GeoTIFF file
    */
  def downloadBand(
      tile: TileId,
    date: LocalDate,
    band: String,
    resolution: Int = 10,
    outputDir: Path = Paths.get("satellite_data")
  ): Try[Path] = {
    Files.createDirectories(outputDir)
    
    val outputFile = outputDir.resolve(s"${tile}_${date}_${band}_${resolution}m.jp2")
    
    // Check cached file
    if (Files.exists(outputFile) && Files.size(outputFile) > 0) {
      val fileSize = Files.size(outputFile)
      println(s"  Using cached: $outputFile (${fileSize / 1024} KB)")
      return Success(outputFile)
    }
    
    // Get OAuth credentials
    val (username, password, clientId, clientSecret) = CopernicusAuth.getCredentialsFromEnv()
    
    if (username.isEmpty && password.isEmpty && clientId.isEmpty && clientSecret.isEmpty) {
      return Failure(new IllegalArgumentException(
        "\n" +
        "╔════════════════════════════════════════════════════════════════╗\n" +
        "║  Copernicus Data Space Authentication Required                ║\n" +
        "╠════════════════════════════════════════════════════════════════╣\n" +
        "║                                                                ║\n" +
        "║  To download real Sentinel-2 data, you need to:               ║\n" +
        "║                                                                ║\n" +
        "║  1. Register at https://dataspace.copernicus.eu/               ║\n" +
        "║  2. Get credentials from:                                      ║\n" +
        "║     https://identity.dataspace.copernicus.eu/auth/realms/CDSE ║\n" +
        "║     /account/                                                  ║\n" +
        "║  3. Set environment variables:                                 ║\n" +
        "║     • COPERNICUS_CLIENT_ID=your_client_id                      ║\n" +
        "║     • COPERNICUS_CLIENT_SECRET=your_client_secret              ║\n" +
        "║                                                                ║\n" +
        "║  Alternatively, use synthetic data mode (works without auth):  ║\n" +
        "║    sbt \"project satellite\" \"runMain io.computenode.cyfra.  ║\n" +
        "║         satellite.examples.quickTest\"                         ║\n" +
        "║                                                                ║\n" +
        "╚════════════════════════════════════════════════════════════════╝\n"
      ))
    }
    
    // Get OAuth token
    val accessToken = CopernicusAuth.getAccessToken(clientId, clientSecret, username, password).getOrElse {
      return Failure(new RuntimeException("Failed to obtain OAuth token"))
    }
    
    // Search for products containing this tile on this date
    val products = CopernicusOData.searchProducts(tile, date, accessToken).getOrElse {
      return Failure(new RuntimeException(s"Failed to find products for tile $tile on $date"))
    }
    
    if (products.isEmpty) {
      return Failure(new RuntimeException(
        s"No Sentinel-2 products found for tile $tile on $date.\n" +
        "Try a different date or tile. Check https://browser.dataspace.copernicus.eu/"
      ))
    }
    
    // Use the first (most recent) product
    val product = products.head
    println(s"  Using product: ${product.Name}")
    
    // Check if product is already cached and extracted
    val productCacheDir = CACHE_DIR.resolve(s"${tile}_${date}_${product.Name.take(20)}")
    val extractedBandPath = productCacheDir.resolve(s"${band}_${resolution}m.jp2")
    
    if (Files.exists(extractedBandPath) && Files.size(extractedBandPath) > 0) {
      println(s"  ✓ Using cached band: $extractedBandPath")
      // Copy to output location
      Files.createDirectories(outputFile.getParent)
      Files.copy(extractedBandPath, outputFile, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      return Success(outputFile)
    }
    
    // Download and extract the product
    println(s"  Downloading product: ${product.Name}")
    println(s"  Size: ${product.ContentLength / 1_000_000} MB")
    println(s"  This may take a few minutes on first download...")
    
    val zipFile = CACHE_DIR.resolve(s"${product.Name}.zip")
    
    // Download ZIP if not already cached
    if (!Files.exists(zipFile) || Files.size(zipFile) < product.ContentLength * 0.9) {
      val downloadUrl = s"$COPERNICUS_DOWNLOAD_API/Products(${product.Id})/$$value"
      println(s"  Downloading from: $downloadUrl")
      
      // Get a fresh token for the download (they expire quickly)
      val (username, password, clientId, clientSecret) = CopernicusAuth.getCredentialsFromEnv()
      val freshToken = CopernicusAuth.getAccessToken(clientId, clientSecret, username, password).getOrElse {
        return Failure(new RuntimeException("Failed to get fresh OAuth token for download"))
      }
      
      CopernicusOData.downloadFile(downloadUrl, zipFile, freshToken) match {
        case Success(_) => println(s"  ✓ Download complete: ${Files.size(zipFile) / 1_000_000} MB")
        case Failure(e) => 
          println(s"  ✗ Download failed: ${e.getMessage}")
          return Failure(e)
      }
    } else {
      println(s"  ✓ Using cached ZIP: $zipFile (${Files.size(zipFile) / 1_000_000} MB)")
    }
    
    // Extract the specific band from ZIP
    println(s"  Extracting band ${band} at ${resolution}m...")
    extractBandFromZip(zipFile, band, resolution, productCacheDir, extractedBandPath) match {
      case Success(_) =>
        println(s"  ✓ Band extracted to: $extractedBandPath")
        // Copy to output location
        Files.createDirectories(outputFile.getParent)
        Files.copy(extractedBandPath, outputFile, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        Success(outputFile)
      case Failure(e) => Failure(e)
    }
  }
  
  /** Extract a specific band from a Sentinel-2 product ZIP file
    *
    * @param zipFile Path to the product ZIP file
    * @param band Band identifier (e.g., "B04")
    * @param resolution Resolution in meters (10, 20, or 60)
    * @param cacheDir Directory to cache extracted files
    * @param outputPath Where to save the extracted band
    * @return Success or failure
    */
  private def extractBandFromZip(
      zipFile: Path,
      band: String,
      resolution: Int,
      cacheDir: Path,
      outputPath: Path
  ): Try[Unit] = Try {
    import java.util.zip.ZipFile
    import java.io.{FileOutputStream, BufferedOutputStream}
    
    Files.createDirectories(cacheDir)
    
    val zip = new ZipFile(zipFile.toFile)
    try {
      // Sentinel-2 structure: GRANULE/*/IMG_DATA/R{resolution}m/*_{band}_{resolution}m.jp2
      val entries = zip.entries().asScala.toList
      
      // Find the band file
      val bandPattern = s".*IMG_DATA.*${band}_${resolution}m\\.jp2$$".r
      val bandEntry = entries.find(e => bandPattern.matches(e.getName)).getOrElse {
        throw new RuntimeException(
          s"Band $band at ${resolution}m not found in product. " +
          s"Available files: ${entries.map(_.getName).filter(_.contains("IMG_DATA")).take(5).mkString(", ")}"
        )
      }
      
      println(s"    Found: ${bandEntry.getName}")
      
      // Extract the file
      val inputStream = zip.getInputStream(bandEntry)
      val outputStream = new BufferedOutputStream(new FileOutputStream(outputPath.toFile))
      
      try {
        val buffer = new Array[Byte](8192)
        var bytesRead = inputStream.read(buffer)
        var totalBytes = 0L
        
        while (bytesRead != -1) {
          outputStream.write(buffer, 0, bytesRead)
          totalBytes += bytesRead
          if (totalBytes % (10 * 1024 * 1024) == 0) {
            print(s"\r    Extracted: ${totalBytes / 1_000_000} MB")
          }
          bytesRead = inputStream.read(buffer)
        }
        println(s"\r    Extracted: ${totalBytes / 1_000_000} MB - Complete!")
      } finally {
        inputStream.close()
        outputStream.close()
      }
    } finally {
      zip.close()
    }
  }
  
  // Initialize ImageIO plugins (needed for JPEG2000 support)
  private lazy val initImageIO: Unit = {
    try {
      // Force plugin discovery
      ImageIO.scanForPlugins()
      val readers = ImageIO.getImageReadersByFormatName("JPEG2000")
      if (!readers.hasNext) {
        println("  WARNING: No JPEG2000 readers found. Available formats: " + ImageIO.getReaderFormatNames.mkString(", "))
      } else {
        println(s"  JPEG2000 reader available: ${readers.next().getClass.getName}")
      }
    } catch {
      case e: Exception =>
        println(s"  WARNING: Error initializing ImageIO: ${e.getMessage}")
    }
  }
  
  /** Load a band from a GeoTIFF or JPEG2000 file
    *
    * Automatically converts large JP2 files to TIFF using GDAL if available
    *
    * @param path Path to the GeoTIFF or JP2 file
    * @param maxDimension Maximum dimension to read (will subsample if larger)
    * @return (data array, width, height)
    */
  def loadBandFromGeoTiff(path: Path, maxDimension: Int = 4096): Try[(Array[Float], Int, Int)] = {
    // For JP2 files, try to convert to TIFF first
    if (path.toString.toLowerCase.endsWith(".jp2")) {
      val tiffPath = Paths.get(path.toString.replace(".jp2", ".tif"))
      
      // Check if TIFF already exists
      if (Files.exists(tiffPath) && Files.size(tiffPath) > 1000) {
        println(s"  Using cached TIFF: ${tiffPath.getFileName}")
        return loadWithImageIO(tiffPath, maxDimension)
      }
      
      // Try to convert with GDAL
      convertJP2toTIFF(path, tiffPath) match {
        case Success(_) =>
          println(s"  ✓ Converted to TIFF: ${tiffPath.getFileName}")
          return loadWithImageIO(tiffPath, maxDimension)
        case Failure(e) =>
          println(s"  WARNING: GDAL conversion failed: ${e.getMessage}")
          println(s"  Attempting to load JP2 directly (may fail for large files)...")
      }
    }
    
    // Load directly with ImageIO
    loadWithImageIO(path, maxDimension)
  }
  
  /** Convert JP2 to TIFF using GDAL command-line tool
    */
  private def convertJP2toTIFF(jp2Path: Path, tiffPath: Path): Try[Unit] = Try {
    // Get GDAL path from system property or environment variable
    val gdalPath = sys.props.get("gdal.path")
      .orElse(sys.env.get("GDAL_PATH"))
      .orElse {
        // Try common installation locations
        val commonPaths = Seq(
          "C:\\Program Files (x86)\\GDAL",
          "C:\\Program Files\\GDAL",
          "/usr/bin",
          "/usr/local/bin"
        )
        commonPaths.find(p => Files.exists(Paths.get(p)))
      }
    
    val gdalCommand = gdalPath match {
      case Some(path) => Paths.get(path, "gdal_translate.exe").toString
      case None => "gdal_translate"  // Hope it's in PATH
    }
    
    // Check if GDAL is available
    val gdalCheck = try {
      new ProcessBuilder(gdalCommand, "--version").start()
    } catch {
      case e: Exception =>
        throw new RuntimeException(
          s"GDAL not found. Set GDAL_PATH environment variable or install from:\n" +
          "  Windows: https://www.gisinternals.com/\n" +
          "  Linux: sudo apt install gdal-bin\n" +
          "  macOS: brew install gdal\n" +
          s"Original error: ${e.getMessage}"
        )
    }
    gdalCheck.waitFor()
    
    if (gdalCheck.exitValue() != 0) {
      throw new RuntimeException("GDAL check failed")
    }
    
    println(s"  Converting JP2 to TIFF with GDAL...")
    
    // Run gdal_translate with UInt16 output
    val command = Seq(
      gdalCommand,
      "-of", "GTiff",
      "-ot", "UInt16",  // Explicitly set output type to 16-bit unsigned
      "-co", "COMPRESS=LZW",
      "-co", "TILED=YES",
      jp2Path.toString,
      tiffPath.toString
    )
    
    val process = new ProcessBuilder(command*)
      .redirectErrorStream(true)
      .start()
    
    val exitCode = process.waitFor()
    
    if (exitCode != 0) {
      // Read error output
      val errorOutput = scala.io.Source.fromInputStream(process.getInputStream).mkString
      throw new RuntimeException(s"GDAL conversion failed (exit code $exitCode): $errorOutput")
    }
    
    if (!Files.exists(tiffPath) || Files.size(tiffPath) == 0) {
      throw new RuntimeException("GDAL conversion produced no output file")
    }
  }
  
  /** Load using standard ImageIO with subsampling
    */
  private def loadWithImageIO(path: Path, maxDimension: Int): Try[(Array[Float], Int, Int)] = Try {
    initImageIO  // Ensure ImageIO plugins are loaded
    
    val fileSize = Files.size(path)
    println(s"  File size: ${fileSize / 1024} KB")
    
    // Create ImageInputStream
    val inputStream: ImageInputStream = ImageIO.createImageInputStream(path.toFile)
    if (inputStream == null) {
      throw new RuntimeException(s"Failed to create ImageInputStream for: $path")
    }
    
    try {
      val readers = ImageIO.getImageReaders(inputStream)
      if (!readers.hasNext) {
        throw new RuntimeException(s"No ImageReader found for: $path")
      }
      
      val reader: ImageReader = readers.next()
      println(s"  Using reader: ${reader.getClass.getSimpleName}")
      
      reader.setInput(inputStream, true, true)
      
      // Get full image dimensions
      val fullWidth = reader.getWidth(0)
      val fullHeight = reader.getHeight(0)
      println(s"  Full image dimensions: ${fullWidth}x${fullHeight}")
      
      // Set up read parameters
      val param = reader.getDefaultReadParam()
      
      // Strategy: Read a centered region or subsample if too large
      if (fullWidth > maxDimension || fullHeight > maxDimension) {
        // Calculate subsampling factor
        val subsampling = math.max(
          (fullWidth + maxDimension - 1) / maxDimension,
          (fullHeight + maxDimension - 1) / maxDimension
        )
        
        println(s"  Image is large, subsampling by ${subsampling}x")
        param.setSourceSubsampling(subsampling, subsampling, 0, 0)
        
        // Alternatively, read just a centered region
        // Uncomment for region-based reading:
        /*
        val regionWidth = math.min(maxDimension, fullWidth)
        val regionHeight = math.min(maxDimension, fullHeight)
        val startX = (fullWidth - regionWidth) / 2
        val startY = (fullHeight - regionHeight) / 2
        
        val sourceRegion = new java.awt.Rectangle(startX, startY, regionWidth, regionHeight)
        param.setSourceRegion(sourceRegion)
        println(s"  Reading region: ${regionWidth}x${regionHeight} at (${startX}, ${startY})")
        */
      }
      
      // Read the image with parameters
      val image = reader.read(0, param)
      
      if (image == null) {
        throw new RuntimeException(s"Reader returned null for: $path")
      }
      
      val width = image.getWidth
      val height = image.getHeight
      val raster = image.getRaster
      val numBands = raster.getNumBands
      
      println(s"  Loaded: ${width}x${height}, bands: $numBands, type: ${image.getType}")
      
      // Convert to float array
      val data = new Array[Float](width * height)
      var idx = 0
      
      if (numBands == 1) {
        // Grayscale image
        for (y <- 0 until height; x <- 0 until width) {
          data(idx) = raster.getSampleFloat(x, y, 0)
          idx += 1
        }
      } else {
        // Multi-band image - take first band
        for (y <- 0 until height; x <- 0 until width) {
          data(idx) = raster.getSampleFloat(x, y, 0)
          idx += 1
        }
      }
      
      (data, width, height)
      
    } finally {
      inputStream.close()
    }
  }
  
  /** Download and load multiple bands to create a SatelliteImage
    *
    * @param tile Tile identifier
    * @param date Date of acquisition
    * @param bands List of band identifiers to download
    * @return SatelliteImage with the requested bands
    */
  def loadSentinel2Scene(
      tile: TileId,
      date: LocalDate,
      bands: List[String] = List("B02", "B03", "B04", "B08"), // Blue, Green, Red, NIR at 10m
      outputDir: Path = Paths.get("satellite_data"),
      targetSize: Int = 1024
  ): Try[SatelliteImage] = {
    println(s"\nLoading Sentinel-2 scene: ${tile} on ${date}")
    
    // Determine resolution for each band
    def bandResolution(band: String): Int = band match {
      case "B02" | "B03" | "B04" | "B08" => 10  // RGB + NIR at 10m
      case "B11" | "B12" => 20  // SWIR at 20m
      case _ => 10
    }
    
    for {
      // Download all bands with appropriate resolution
      bandPaths <- bands.map { band =>
        val resolution = bandResolution(band)
        downloadBand(tile, date, band, resolution, outputDir)
      }.foldLeft(
        Success(List.empty[Path]): Try[List[Path]]
      ) { (acc, result) =>
        for {
          list <- acc
          path <- result
        } yield list :+ path
      }
      
      // Load first band to get dimensions
      firstBand <- loadBandFromGeoTiff(bandPaths.head, targetSize)
      (_, width, height) = firstBand
      
      // Load all bands and resample to match dimensions
      bandData <- bandPaths.zip(bands).map { case (path, bandName) =>
        loadBandFromGeoTiff(path, targetSize).map { case (data, w, h) =>
          // Resample if dimensions don't match (e.g., 20m SWIR to 10m resolution)
          val resampledData = if (w != width || h != height) {
            println(s"  Resampling ${bandName} from ${w}x${h} to ${width}x${height}")
            resampleBand(data, w, h, width, height)
          } else {
            data
          }
          
          val bandKey = bandName match {
            case "B02" => "Blue"
            case "B03" => "Green"
            case "B04" => "Red"
            case "B08" => "NIR"
            case "B11" | "B12" => "SWIR"
            case other => throw new IllegalArgumentException(s"Unknown band: $other")
          }
          bandKey -> resampledData
        }
      }.foldLeft(Success(Map.empty[String, Array[Float]]): Try[Map[String, Array[Float]]]) {
        (acc, result) =>
          for {
            map <- acc
            (band, data) <- result
          } yield map + (band -> data)
      }
      
    } yield {
      println(s"✓ Loaded ${bands.size} bands: ${width}x${height} pixels")
      val metadata = ImageMetadata(
        satellite = "Sentinel-2",
        acquisitionTime = date.atStartOfDay(),
        processingLevel = "L2A",
        tileId = Some(s"${tile.toString}")
      )
      SatelliteImage(bandData, width, height, metadata)
    }
  }
  
  /** Resample a band to a different resolution using nearest neighbor interpolation */
  private def resampleBand(
      data: Array[Float],
      srcWidth: Int,
      srcHeight: Int,
      dstWidth: Int,
      dstHeight: Int
  ): Array[Float] = {
    val result = Array.ofDim[Float](dstWidth * dstHeight)
    val scaleX = srcWidth.toFloat / dstWidth
    val scaleY = srcHeight.toFloat / dstHeight
    
    for (y <- 0 until dstHeight; x <- 0 until dstWidth) {
      val srcX = math.min((x * scaleX).toInt, srcWidth - 1)
      val srcY = math.min((y * scaleY).toInt, srcHeight - 1)
      result(y * dstWidth + x) = data(srcY * srcWidth + srcX)
    }
    
    result
  }
  
  /** Get available dates for a tile (simplified - would need to parse S3 XML listing in production) */
  def getAvailableDates(tile: TileId, year: Int, month: Int): Try[List[LocalDate]] = Try {
    // This is a simplified version - in production you'd parse the S3 bucket listing
    // For now, return some common dates
    (1 to 28).map(day => LocalDate.of(year, month, day)).toList
  }

