package io.computenode.cyfra.satellite.data

import java.nio.file.Path
import java.time.LocalDateTime

/** Represents a multispectral satellite image with metadata.
  *
  * @param bands Map of band identifiers to their pixel data (flattened row-major arrays)
  * @param width Image width in pixels
  * @param height Image height in pixels
  * @param metadata Image metadata
  */
case class SatelliteImage(
    bands: Map[String, Array[Float]],
    width: Int,
    height: Int,
    metadata: ImageMetadata
):
  require(width > 0, "Width must be positive")
  require(height > 0, "Height must be positive")
  require(bands.nonEmpty, "Must have at least one band")
  
  val pixelCount: Int = width * height
  
  // Verify all bands have correct size
  bands.foreach { case (name, data) =>
    require(
      data.length == pixelCount,
      s"Band $name has ${data.length} pixels but expected $pixelCount"
    )
  }

  /** Get pixel value from a specific band at (x, y) coordinates */
  def getPixel(band: String, x: Int, y: Int): Float =
    require(x >= 0 && x < width, s"x coordinate $x out of bounds [0, $width)")
    require(y >= 0 && y < height, s"y coordinate $y out of bounds [0, $height)")
    bands(band)(y * width + x)

  /** Set pixel value in a specific band at (x, y) coordinates */
  def setPixel(band: String, x: Int, y: Int, value: Float): Unit =
    require(x >= 0 && x < width, s"x coordinate $x out of bounds [0, $width)")
    require(y >= 0 && y < height, s"y coordinate $y out of bounds [0, $height)")
    bands(band)(y * width + x) = value

  /** Get all available band names */
  def bandNames: Set[String] = bands.keySet

  /** Check if a specific band exists */
  def hasBand(name: String): Boolean = bands.contains(name)

  /** Extract a subset of bands */
  def selectBands(bandNames: String*): SatelliteImage =
    val selectedBands = bandNames.map(name => name -> bands(name)).toMap
    copy(bands = selectedBands)

  /** Create a new image with an additional computed band */
  def withBand(name: String, data: Array[Float]): SatelliteImage =
    require(
      data.length == pixelCount,
      s"Band data has ${data.length} pixels but expected $pixelCount"
    )
    copy(bands = bands + (name -> data))

/** Metadata for satellite imagery */
case class ImageMetadata(
    /** Source satellite/sensor name (e.g., "Sentinel-2A") */
    satellite: String,
    /** Acquisition timestamp */
    acquisitionTime: LocalDateTime,
    /** Processing level (e.g., "L2A") */
    processingLevel: String,
    /** Tile identifier (e.g., "T32TPS") */
    tileId: Option[String] = None,
    /** Cloud coverage percentage (0-100) */
    cloudCoverage: Option[Double] = None,
    /** Sun azimuth angle in degrees */
    sunAzimuth: Option[Double] = None,
    /** Sun elevation angle in degrees */
    sunElevation: Option[Double] = None,
    /** Geographic bounds (min_lon, min_lat, max_lon, max_lat) */
    bounds: Option[(Double, Double, Double, Double)] = None,
    /** Coordinate reference system (e.g., "EPSG:32632") */
    crs: Option[String] = None,
    /** Additional custom metadata */
    custom: Map[String, String] = Map.empty
):
  /** Check if image has acceptable cloud coverage for analysis */
  def hasLowCloudCoverage(threshold: Double = 10.0): Boolean =
    cloudCoverage.forall(_ < threshold)

  /** Check if sun angle is suitable for analysis (not too low) */
  def hasSufficientSunElevation(minElevation: Double = 20.0): Boolean =
    sunElevation.forall(_ > minElevation)

/** Result of a spectral index calculation */
case class SpectralIndexResult(
    /** The computed index values (flattened row-major array) */
    values: Array[Float],
    /** Image dimensions */
    width: Int,
    height: Int,
    /** Index name (e.g., "NDVI", "EVI") */
    indexName: String,
    /** Source image metadata */
    sourceMetadata: ImageMetadata
):
  val pixelCount: Int = width * height
  require(
    values.length == pixelCount,
    s"Index data has ${values.length} pixels but expected $pixelCount"
  )

  /** Get index value at (x, y) coordinates */
  def getValue(x: Int, y: Int): Float =
    require(x >= 0 && x < width, s"x coordinate $x out of bounds [0, $width)")
    require(y >= 0 && y < height, s"y coordinate $y out of bounds [0, $height)")
    values(y * width + x)

  /** Compute statistics over the entire image */
  def statistics: IndexStatistics =
    var sum = 0.0
    var min = Float.MaxValue
    var max = Float.MinValue
    var validCount = 0

    values.foreach { v =>
      if (!v.isNaN && !v.isInfinity) {
        sum += v
        min = math.min(min, v)
        max = math.max(max, v)
        validCount += 1
      }
    }

    val mean = if (validCount > 0) sum / validCount else 0.0
    
    // Compute standard deviation
    var sumSquaredDiff = 0.0
    values.foreach { v =>
      if (!v.isNaN && !v.isInfinity) {
        val diff = v - mean
        sumSquaredDiff += diff * diff
      }
    }
    val stdDev = if (validCount > 0) math.sqrt(sumSquaredDiff / validCount) else 0.0

    IndexStatistics(
      mean = mean.toFloat,
      stdDev = stdDev.toFloat,
      min = if (validCount > 0) min else 0.0f,
      max = if (validCount > 0) max else 0.0f,
      validPixels = validCount,
      totalPixels = pixelCount
    )

  /** Create a thresholded binary mask */
  def threshold(min: Float, max: Float): Array[Boolean] =
    values.map(v => v >= min && v <= max)

/** Statistical summary of a spectral index */
case class IndexStatistics(
    mean: Float,
    stdDev: Float,
    min: Float,
    max: Float,
    validPixels: Int,
    totalPixels: Int
):
  def validPercentage: Double = (validPixels.toDouble / totalPixels) * 100.0

/** Time series of satellite images for temporal analysis */
case class ImageTimeSeries(
    images: List[(LocalDateTime, SatelliteImage)]
):
  require(images.nonEmpty, "Time series must contain at least one image")
  
  /** Images sorted by acquisition time */
  lazy val sorted: List[(LocalDateTime, SatelliteImage)] =
    images.sortBy(_._1)

  /** Get images within a time range */
  def filterByDate(start: LocalDateTime, end: LocalDateTime): ImageTimeSeries =
    ImageTimeSeries(images.filter { case (time, _) =>
      !time.isBefore(start) && !time.isAfter(end)
    })

  /** Get the temporal span of this time series */
  def timeSpan: (LocalDateTime, LocalDateTime) =
    val times = images.map(_._1)
    (times.min, times.max)





