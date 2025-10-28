package io.computenode.cyfra.satellite.spectral

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.library.Functions

/** GPU-accelerated spectral index calculations for multispectral satellite imagery.
  *
  * These indices are computed in parallel for each pixel on the GPU, enabling real-time
  * analysis of large satellite datasets that would be prohibitively slow on CPU.
  *
  * All indices return Float32 values that can be used for vegetation, water, urban area,
  * and fire detection in satellite imagery.
  */
object SpectralIndices:

  /** Normalized Difference Vegetation Index (NDVI)
    *
    * Measures vegetation health and density. Healthy vegetation reflects strongly in
    * near-infrared (NIR) and absorbs red light for photosynthesis.
    *
    * Formula: (NIR - Red) / (NIR + Red)
    *
    * Range: -1.0 to 1.0
    * - Water: -1.0 to 0.0
    * - Bare soil/rocks: 0.0 to 0.1
    * - Sparse vegetation: 0.1 to 0.3
    * - Moderate vegetation: 0.3 to 0.6
    * - Dense vegetation: 0.6 to 1.0
    *
    * @param nir Near-infrared band reflectance (Sentinel-2 Band 8)
    * @param red Red band reflectance (Sentinel-2 Band 4)
    * @return NDVI value
    */
  def ndvi(nir: Float32, red: Float32): Float32 =
    val numerator = nir - red
    val denominator = nir + red
    // Avoid division by zero
    when(Functions.abs(denominator) < 0.0001f)(
      0.0f
    ).otherwise(
      numerator / denominator
    )

  /** Enhanced Vegetation Index (EVI)
    *
    * Improved vegetation index that corrects for atmospheric conditions and canopy
    * background noise. More accurate than NDVI in areas with high vegetation density.
    *
    * Formula: 2.5 * ((NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1))
    *
    * Range: -1.0 to 1.0
    * - Optimized for high biomass regions
    * - Better sensitivity in dense vegetation
    * - Reduced atmospheric influence
    *
    * @param nir Near-infrared band (Sentinel-2 Band 8)
    * @param red Red band (Sentinel-2 Band 4)
    * @param blue Blue band (Sentinel-2 Band 2)
    * @return EVI value
    */
  def evi(nir: Float32, red: Float32, blue: Float32): Float32 =
    val numerator = nir - red
    val denominator = nir + 6.0f * red - 7.5f * blue + 1.0f
    when(Functions.abs(denominator) < 0.0001f)(
      0.0f
    ).otherwise(
      2.5f * (numerator / denominator)
    )

  /** Normalized Difference Water Index (NDWI)
    *
    * Detects water bodies and measures water content in vegetation. Water reflects
    * strongly in green band and absorbs NIR.
    *
    * Formula: (Green - NIR) / (Green + NIR)
    *
    * Range: -1.0 to 1.0
    * - Water bodies: > 0.3
    * - Wet vegetation: 0.0 to 0.3
    * - Dry vegetation/soil: < 0.0
    *
    * @param green Green band (Sentinel-2 Band 3)
    * @param nir Near-infrared band (Sentinel-2 Band 8)
    * @return NDWI value
    */
  def ndwi(green: Float32, nir: Float32): Float32 =
    val numerator = green - nir
    val denominator = green + nir
    when(Functions.abs(denominator) < 0.0001f)(
      0.0f
    ).otherwise(
      numerator / denominator
    )

  /** Normalized Difference Built-up Index (NDBI)
    *
    * Detects urban/built-up areas and bare land. Built-up areas have higher reflectance
    * in SWIR (shortwave infrared) than NIR.
    *
    * Formula: (SWIR - NIR) / (SWIR + NIR)
    *
    * Range: -1.0 to 1.0
    * - Urban/built-up areas: > 0.0
    * - Vegetation: < 0.0
    *
    * @param swir Shortwave infrared band (Sentinel-2 Band 11)
    * @param nir Near-infrared band (Sentinel-2 Band 8)
    * @return NDBI value
    */
  def ndbi(swir: Float32, nir: Float32): Float32 =
    val numerator = swir - nir
    val denominator = swir + nir
    when(Functions.abs(denominator) < 0.0001f)(
      0.0f
    ).otherwise(
      numerator / denominator
    )

  /** Normalized Burn Ratio (NBR)
    *
    * Detects burned areas and assesses fire severity. Healthy vegetation reflects
    * strongly in NIR and absorbs SWIR, while burned areas show the opposite pattern.
    *
    * Formula: (NIR - SWIR) / (NIR + SWIR)
    *
    * Range: -1.0 to 1.0
    * - Healthy vegetation: 0.4 to 1.0
    * - Moderate burn: 0.1 to 0.4
    * - Severe burn: -0.4 to 0.1
    * - Water/bare ground: -1.0 to -0.4
    *
    * dNBR (difference NBR) between pre and post-fire images is used for burn severity:
    * - Unburned: dNBR < 0.1
    * - Low severity: 0.1 to 0.27
    * - Moderate-low: 0.27 to 0.44
    * - Moderate-high: 0.44 to 0.66
    * - High severity: > 0.66
    *
    * @param nir Near-infrared band (Sentinel-2 Band 8)
    * @param swir Shortwave infrared band (Sentinel-2 Band 12)
    * @return NBR value
    */
  def nbr(nir: Float32, swir: Float32): Float32 =
    val numerator = nir - swir
    val denominator = nir + swir
    when(Functions.abs(denominator) < 0.0001f)(
      0.0f
    ).otherwise(
      numerator / denominator
    )

  /** Soil Adjusted Vegetation Index (SAVI)
    *
    * Modified vegetation index that minimizes soil brightness influences. Particularly
    * useful in areas with sparse vegetation or early growth stages.
    *
    * Formula: ((NIR - Red) / (NIR + Red + L)) * (1 + L)
    * where L is a soil brightness correction factor (typically 0.5)
    *
    * Range: -1.0 to 1.0
    * - Better than NDVI for areas with <40% vegetation cover
    * - L=0.5 for intermediate vegetation densities
    * - L=1.0 for very sparse vegetation
    * - L=0.0 for dense vegetation (equivalent to NDVI)
    *
    * @param nir Near-infrared band (Sentinel-2 Band 8)
    * @param red Red band (Sentinel-2 Band 4)
    * @param l Soil brightness correction factor (default 0.5)
    * @return SAVI value
    */
  def savi(nir: Float32, red: Float32, l: Float32 = 0.5f): Float32 =
    val numerator = nir - red
    val denominator = nir + red + l
    when(Functions.abs(denominator) < 0.0001f)(
      0.0f
    ).otherwise(
      (numerator / denominator) * (1.0f + l)
    )

  /** Modified Normalized Difference Water Index (MNDWI)
    *
    * Enhanced water index using SWIR instead of NIR. Better at separating water from
    * built-up areas and reduces noise from built-up land.
    *
    * Formula: (Green - SWIR) / (Green + SWIR)
    *
    * Range: -1.0 to 1.0
    * - Water: > 0.0
    * - Non-water: < 0.0
    * - More accurate for water extraction than NDWI
    *
    * @param green Green band (Sentinel-2 Band 3)
    * @param swir Shortwave infrared band (Sentinel-2 Band 11)
    * @return MNDWI value
    */
  def mndwi(green: Float32, swir: Float32): Float32 =
    val numerator = green - swir
    val denominator = green + swir
    when(Functions.abs(denominator) < 0.0001f)(
      0.0f
    ).otherwise(
      numerator / denominator
    )

  /** Bare Soil Index (BSI)
    *
    * Detects and quantifies bare soil areas. Useful for monitoring soil erosion,
    * desertification, and agricultural land preparation.
    *
    * Formula: ((SWIR + Red) - (NIR + Blue)) / ((SWIR + Red) + (NIR + Blue))
    *
    * Range: -1.0 to 1.0
    * - Higher values indicate bare soil
    * - Lower values indicate vegetation or water
    *
    * @param swir Shortwave infrared band (Sentinel-2 Band 11)
    * @param red Red band (Sentinel-2 Band 4)
    * @param nir Near-infrared band (Sentinel-2 Band 8)
    * @param blue Blue band (Sentinel-2 Band 2)
    * @return BSI value
    */
  def bsi(swir: Float32, red: Float32, nir: Float32, blue: Float32): Float32 =
    val sum1 = swir + red
    val sum2 = nir + blue
    val numerator = sum1 - sum2
    val denominator = sum1 + sum2
    when(Functions.abs(denominator) < 0.0001f)(
      0.0f
    ).otherwise(
      numerator / denominator
    )

  /** Change Detection Index
    *
    * Computes the absolute difference between two spectral index values from different
    * time periods. Used for temporal change detection.
    *
    * @param before Index value before change
    * @param after Index value after change
    * @return Absolute change magnitude
    */
  def changeIndex(before: Float32, after: Float32): Float32 =
    Functions.abs(after - before)

  /** Relative Change Detection
    *
    * Computes relative change as a percentage. More useful than absolute change when
    * comparing areas with different baseline values.
    *
    * @param before Index value before change
    * @param after Index value after change
    * @return Relative change (-1.0 to 1.0, where 1.0 = 100% increase)
    */
  def relativeChange(before: Float32, after: Float32): Float32 =
    when(Functions.abs(before) < 0.0001f)(
      when(Functions.abs(after) < 0.0001f)(
        0.0f
      ).otherwise(
        1.0f // Maximum change if before is zero but after is not
      )
    ).otherwise(
      (after - before) / Functions.abs(before)
    )


