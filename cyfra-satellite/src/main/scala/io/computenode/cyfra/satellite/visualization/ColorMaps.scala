package io.computenode.cyfra.satellite.visualization

import java.awt.Color

/** Color mapping functions for visualizing spectral indices as heat maps */
object ColorMaps:

  /** RGB color tuple */
  type RGB = (Int, Int, Int)

  /** Interpolate between two colors based on a value in [0, 1] */
  private def interpolate(c1: RGB, c2: RGB, t: Float): RGB =
    val tClamped = Math.max(0.0f, Math.min(1.0f, t))
    val r = (c1._1 + (c2._1 - c1._1) * tClamped).toInt
    val g = (c1._2 + (c2._2 - c1._2) * tClamped).toInt
    val b = (c1._3 + (c2._3 - c1._3) * tClamped).toInt
    (r, g, b)

  /** Normalize a value from [min, max] to [0, 1] */
  private def normalize(value: Float, min: Float, max: Float): Float =
    if (max - min < 0.0001f) 0.5f
    else {
      val normalized = (value - min) / (max - min)
      Math.max(0.0f, Math.min(1.0f, normalized))
    }

  /** Viridis color map (perceptually uniform, colorblind-friendly)
    *
    * Good for: General purpose spectral indices
    */
  def viridis(value: Float, min: Float = -1.0f, max: Float = 1.0f): RGB =
    val t = normalize(value, min, max)
    
    if (t < 0.25f) {
      interpolate((68, 1, 84), (59, 82, 139), t * 4)
    } else if (t < 0.5f) {
      interpolate((59, 82, 139), (33, 145, 140), (t - 0.25f) * 4)
    } else if (t < 0.75f) {
      interpolate((33, 145, 140), (94, 201, 98), (t - 0.5f) * 4)
    } else {
      interpolate((94, 201, 98), (253, 231, 37), (t - 0.75f) * 4)
    }

  /** Vegetation color map (brown to green)
    *
    * Good for: NDVI, EVI, vegetation indices
    */
  def vegetation(value: Float, min: Float = -1.0f, max: Float = 1.0f): RGB =
    val t = normalize(value, min, max)
    
    if (t < 0.2f) {
      // Water (blue)
      interpolate((0, 0, 139), (173, 216, 230), t * 5)
    } else if (t < 0.4f) {
      // Bare soil (light blue to brown)
      interpolate((173, 216, 230), (139, 90, 43), (t - 0.2f) * 5)
    } else if (t < 0.6f) {
      // Sparse vegetation (brown to yellow-green)
      interpolate((139, 90, 43), (154, 205, 50), (t - 0.4f) * 5)
    } else {
      // Dense vegetation (yellow-green to dark green)
      interpolate((154, 205, 50), (0, 100, 0), (t - 0.6f) * 2.5f)
    }

  /** Water color map (brown to blue)
    *
    * Good for: NDWI, MNDWI water indices
    */
  def water(value: Float, min: Float = -1.0f, max: Float = 1.0f): RGB =
    val t = normalize(value, min, max)
    
    if (t < 0.5f) {
      // Land (brown to tan)
      interpolate((139, 69, 19), (222, 184, 135), t * 2)
    } else {
      // Water (light blue to deep blue)
      interpolate((135, 206, 235), (0, 0, 139), (t - 0.5f) * 2)
    }

  /** Urban color map (green to gray)
    *
    * Good for: NDBI, urban area detection
    */
  def urban(value: Float, min: Float = -1.0f, max: Float = 1.0f): RGB =
    val t = normalize(value, min, max)
    
    if (t < 0.5f) {
      // Vegetation (dark green to light green)
      interpolate((0, 100, 0), (144, 238, 144), t * 2)
    } else {
      // Built-up (light gray to dark gray)
      interpolate((192, 192, 192), (64, 64, 64), (t - 0.5f) * 2)
    }

  /** Fire/burn color map (green to red/black)
    *
    * Good for: NBR, burn severity
    */
  def burn(value: Float, min: Float = -1.0f, max: Float = 1.0f): RGB =
    val t = normalize(value, min, max)
    
    if (t < 0.25f) {
      // Severely burned (black to dark red)
      interpolate((0, 0, 0), (139, 0, 0), t * 4)
    } else if (t < 0.5f) {
      // Moderately burned (dark red to orange)
      interpolate((139, 0, 0), (255, 140, 0), (t - 0.25f) * 4)
    } else if (t < 0.75f) {
      // Lightly burned (orange to yellow)
      interpolate((255, 140, 0), (255, 255, 0), (t - 0.5f) * 4)
    } else {
      // Healthy vegetation (yellow to green)
      interpolate((255, 255, 0), (0, 128, 0), (t - 0.75f) * 4)
    }

  /** Change detection color map (blue-white-red diverging)
    *
    * Good for: Temporal change detection
    */
  def change(value: Float, min: Float = -1.0f, max: Float = 1.0f): RGB =
    val t = normalize(value, min, max)
    
    if (t < 0.5f) {
      // Negative change (dark blue to white)
      interpolate((0, 0, 139), (255, 255, 255), t * 2)
    } else {
      // Positive change (white to dark red)
      interpolate((255, 255, 255), (139, 0, 0), (t - 0.5f) * 2)
    }

  /** Jet color map (rainbow)
    *
    * Good for: High dynamic range visualization
    * Note: Not colorblind-friendly, use sparingly
    */
  def jet(value: Float, min: Float = -1.0f, max: Float = 1.0f): RGB =
    val t = normalize(value, min, max)
    
    if (t < 0.25f) {
      interpolate((0, 0, 139), (0, 0, 255), t * 4)
    } else if (t < 0.5f) {
      interpolate((0, 0, 255), (0, 255, 255), (t - 0.25f) * 4)
    } else if (t < 0.75f) {
      interpolate((0, 255, 255), (255, 255, 0), (t - 0.5f) * 4)
    } else {
      interpolate((255, 255, 0), (255, 0, 0), (t - 0.75f) * 4)
    }

  /** Grayscale color map
    *
    * Good for: Simple intensity visualization
    */
  def grayscale(value: Float, min: Float = -1.0f, max: Float = 1.0f): RGB =
    val t = normalize(value, min, max)
    val intensity = (t * 255).toInt
    (intensity, intensity, intensity)

  /** Get the appropriate color map for a spectral index */
  def forIndex(indexName: String): (Float, Float, Float) => RGB =
    indexName.toUpperCase match
      case "NDVI" | "EVI" | "SAVI" => vegetation
      case "NDWI" | "MNDWI" => water
      case "NDBI" | "BSI" => urban
      case "NBR" => burn
      case name if name.contains("CHANGE") => change
      case _ => viridis

  /** Create a legend for a color map */
  def createLegend(
      colorMap: (Float, Float, Float) => RGB,
      min: Float,
      max: Float,
      steps: Int = 256
  ): Array[RGB] =
    Array.tabulate(steps) { i =>
      val value = min + (max - min) * i / (steps - 1).toFloat
      colorMap(value, min, max)
    }


