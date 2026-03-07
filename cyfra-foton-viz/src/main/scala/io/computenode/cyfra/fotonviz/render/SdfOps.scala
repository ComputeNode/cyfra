package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}

/**
 * SDF combination and modification operations.
 *
 * Supports both raw Float32 distances and SdfResult with material blending.
 */
object SdfOps:

  // ==== Raw Distance Operations (Float32) ====

  /** Union: combine two SDFs (min). */
  def opUnion(d1: Float32, d2: Float32): Float32 = min(d1, d2)

  /** Intersection: overlap of two SDFs (max). */
  def opIntersection(d1: Float32, d2: Float32): Float32 = max(d1, d2)

  /** Subtraction: d1 minus d2. */
  def opSubtraction(d1: Float32, d2: Float32): Float32 = max(d1, -d2)

  /** Smooth union with blending factor k. */
  def opSmoothUnion(d1: Float32, d2: Float32, k: Float32): Float32 =
    val h = clamp(0.5f + 0.5f * (d2 - d1) / k, 0.0f, 1.0f)
    d2 * (1.0f - h) + d1 * h - k * h * (1.0f - h)

  /** Smooth subtraction with blending factor k. */
  def opSmoothSubtraction(d1: Float32, d2: Float32, k: Float32): Float32 =
    val h = clamp(0.5f - 0.5f * (d2 + d1) / k, 0.0f, 1.0f)
    d1 * (1.0f - h) + (-d2) * h + k * h * (1.0f - h)

  /** Smooth intersection with blending factor k. */
  def opSmoothIntersection(d1: Float32, d2: Float32, k: Float32): Float32 =
    val h = clamp(0.5f - 0.5f * (d2 - d1) / k, 0.0f, 1.0f)
    d2 * (1.0f - h) + d1 * h + k * h * (1.0f - h)

  /** Displacement: add displacement to base SDF. */
  def opDisplace(baseSdf: Float32, displacement: Float32): Float32 =
    baseSdf + displacement

  /** Round: soften edges by subtracting radius. */
  def opRound(sdf: Float32, radius: Float32): Float32 =
    sdf - radius

  /** Onion: create a shell of given thickness. */
  def opOnion(sdf: Float32, thickness: Float32): Float32 =
    abs(sdf) - thickness

  // ==== SdfResult Operations (with Material Blending) ====

  /**
   * Union of two SdfResults - takes the closer surface.
   * Materials are selected from the closer surface.
   */
  def union(a: SdfResult, b: SdfResult): SdfResult =
    val aCloser = a.distance < b.distance
    SdfResult(
      distance = when(aCloser)(a.distance).otherwise(b.distance),
      materials = when(aCloser)(a.materials).otherwise(b.materials),
    )

  /**
   * Smooth union with automatic material blending.
   *
   * Materials are interpolated based on distance ratio in the blend zone.
   *
   * @param a First SDF result
   * @param b Second SDF result
   * @param k Blend factor (larger = smoother blend)
   */
  def smoothUnion(a: SdfResult, b: SdfResult, k: Float32): SdfResult =
    val h = clamp(0.5f + 0.5f * (b.distance - a.distance) / k, 0.0f, 1.0f)
    val blendedDist = b.distance * (1.0f - h) + a.distance * h - k * h * (1.0f - h)
    val blendedMats = SdfResult.blendMaterials(a.materials, b.materials, 1.0f - h)
    SdfResult(blendedDist, blendedMats)

  /**
   * Intersection of two SdfResults.
   * Materials are selected from the further surface (the one that defines the edge).
   */
  def intersection(a: SdfResult, b: SdfResult): SdfResult =
    val aFurther = a.distance > b.distance
    SdfResult(
      distance = when(aFurther)(a.distance).otherwise(b.distance),
      materials = when(aFurther)(a.materials).otherwise(b.materials),
    )

  /**
   * Smooth intersection with material blending.
   */
  def smoothIntersection(a: SdfResult, b: SdfResult, k: Float32): SdfResult =
    val h = clamp(0.5f - 0.5f * (b.distance - a.distance) / k, 0.0f, 1.0f)
    val blendedDist = b.distance * (1.0f - h) + a.distance * h + k * h * (1.0f - h)
    val blendedMats = SdfResult.blendMaterials(b.materials, a.materials, h)
    SdfResult(blendedDist, blendedMats)

  /**
   * Subtraction: a minus b.
   * Materials come from surface a.
   */
  def subtraction(a: SdfResult, b: SdfResult): SdfResult =
    val negB = -b.distance
    val aCloser = a.distance > negB
    SdfResult(
      distance = when(aCloser)(a.distance).otherwise(negB),
      materials = a.materials, // Always use material from the positive shape
    )

  /**
   * Smooth subtraction with material blending.
   */
  def smoothSubtraction(a: SdfResult, b: SdfResult, k: Float32): SdfResult =
    val h = clamp(0.5f - 0.5f * (b.distance + a.distance) / k, 0.0f, 1.0f)
    val blendedDist = a.distance * (1.0f - h) + (-b.distance) * h + k * h * (1.0f - h)
    SdfResult(blendedDist, a.materials)

  /**
   * Apply displacement to an SdfResult.
   */
  def displace(sdf: SdfResult, displacement: Float32): SdfResult =
    SdfResult(sdf.distance + displacement, sdf.materials)

  /**
   * Round an SdfResult.
   */
  def round(sdf: SdfResult, radius: Float32): SdfResult =
    SdfResult(sdf.distance - radius, sdf.materials)

  /**
   * Create shell from SdfResult.
   */
  def onion(sdf: SdfResult, thickness: Float32): SdfResult =
    SdfResult(abs(sdf.distance) - thickness, sdf.materials)
