package io.computenode.cyfra.satellite.data

/** Sentinel-2 spectral band definitions and characteristics.
  *
  * Sentinel-2 carries the Multispectral Instrument (MSI) with 13 spectral bands from
  * visible to shortwave infrared (SWIR) with different spatial resolutions.
  */
object Sentinel2Bands:

  /** Sentinel-2 band characteristics */
  enum Band(
      val number: Int,
      val name: String,
      val centralWavelength: Double, // nanometers
      val bandwidth: Double, // nanometers
      val resolution: Int // meters
  ):
    case B1 extends Band(1, "Coastal aerosol", 443, 20, 60)
    case B2 extends Band(2, "Blue", 490, 65, 10)
    case B3 extends Band(3, "Green", 560, 35, 10)
    case B4 extends Band(4, "Red", 665, 30, 10)
    case B5 extends Band(5, "Red Edge 1", 705, 15, 20)
    case B6 extends Band(6, "Red Edge 2", 740, 15, 20)
    case B7 extends Band(7, "Red Edge 3", 783, 20, 20)
    case B8 extends Band(8, "NIR", 842, 115, 10)
    case B8A extends Band(8, "NIR narrow", 865, 20, 20)
    case B9 extends Band(9, "Water vapor", 945, 20, 60)
    case B10 extends Band(10, "SWIR - Cirrus", 1375, 30, 60)
    case B11 extends Band(11, "SWIR 1", 1610, 90, 20)
    case B12 extends Band(12, "SWIR 2", 2190, 180, 20)

  /** Common band aliases for spectral index calculations */
  object CommonBands:
    val Blue = Band.B2
    val Green = Band.B3
    val Red = Band.B4
    val NIR = Band.B8
    val SWIR1 = Band.B11
    val SWIR2 = Band.B12

  /** Band combinations for common applications */
  object BandCombinations:
    /** True color composite (as seen by human eye) */
    val TrueColor = (Band.B4, Band.B3, Band.B2) // Red, Green, Blue

    /** False color composite (vegetation appears red) */
    val FalseColor = (Band.B8, Band.B4, Band.B3) // NIR, Red, Green

    /** False color urban (urban areas appear cyan/blue) */
    val FalseColorUrban = (Band.B12, Band.B11, Band.B4) // SWIR2, SWIR1, Red

    /** Agriculture (vegetation health visualization) */
    val Agriculture = (Band.B11, Band.B8, Band.B2) // SWIR1, NIR, Blue

    /** Atmospheric penetration (reduces atmospheric effects) */
    val AtmosphericPenetration = (Band.B12, Band.B11, Band.B8A) // SWIR2, SWIR1, NIR narrow

    /** Healthy vegetation (chlorophyll content) */
    val HealthyVegetation = (Band.B8, Band.B11, Band.B2) // NIR, SWIR1, Blue

    /** Land/water boundary */
    val LandWater = (Band.B8, Band.B11, Band.B4) // NIR, SWIR1, Red

    /** Natural color with atmospheric removal */
    val NaturalColor = (Band.B12, Band.B8, Band.B3) // SWIR2, NIR, Green

    /** Shortwave infrared (penetrates haze, smoke) */
    val SWIR = (Band.B12, Band.B8, Band.B4) // SWIR2, NIR, Red

    /** Geology (differentiates rock types) */
    val Geology = (Band.B12, Band.B11, Band.B2) // SWIR2, SWIR1, Blue

  /** Processing levels available from Copernicus Hub */
  enum ProcessingLevel:
    case L1C // Top-of-atmosphere reflectance
    case L2A // Bottom-of-atmosphere reflectance (atmospherically corrected)

  /** Product types */
  sealed trait ProductType:
    def level: ProcessingLevel
    def tileId: String

  case class L1CProduct(tileId: String) extends ProductType:
    val level = ProcessingLevel.L1C

  case class L2AProduct(tileId: String) extends ProductType:
    val level = ProcessingLevel.L2A

  /** Sentinel-2 tile naming format: TXXXXX
    *
    * Example: T32TPS where:
    * - 32 = UTM zone
    * - T = latitude band
    * - PS = grid square
    */
  case class TileId(value: String):
    require(value.matches("T[0-9]{2}[A-Z]{3}"), s"Invalid tile ID: $value")

    def utmZone: Int = value.substring(1, 3).toInt
    def latitudeBand: Char = value.charAt(3)
    def gridSquare: String = value.substring(4, 6)

  /** Sentinel-2 product naming convention
    *
    * Format: MMM_MSIL2A_YYYYMMDDHHMMSS_Nxxyy_ROOO_TXXXXX_<Product Discriminator>.SAFE
    *
    * Example:
    * S2B_MSIL2A_20231015T103859_N0509_R008_T32TPS_20231015T142156.SAFE
    */
  case class ProductName(value: String):
    private val parts = value.split("_")
    
    def satellite: String = parts(0) // S2A or S2B
    def processingLevel: String = parts(1) // MSIL1C or MSIL2A
    def sensingTime: String = parts(2)
    def processingBaseline: String = parts(3)
    def relativeOrbit: String = parts(4)
    def tileId: TileId = TileId(parts(5))

  /** Resolution groups for efficient processing */
  enum ResolutionGroup(val bands: List[Band], val resolution: Int):
    case High extends ResolutionGroup(List(Band.B2, Band.B3, Band.B4, Band.B8), 10)
    case Medium extends ResolutionGroup(
      List(Band.B5, Band.B6, Band.B7, Band.B8A, Band.B11, Band.B12),
      20
    )
    case Low extends ResolutionGroup(List(Band.B1, Band.B9, Band.B10), 60)





