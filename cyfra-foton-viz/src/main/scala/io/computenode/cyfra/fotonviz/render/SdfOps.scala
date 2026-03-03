package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}

/** SDF combination and modification operations (IQ-style). */
object SdfOps:

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

  /** Displacement: add displacement to base SDF (preserves SDF properties if bounded). */
  def opDisplace(baseSdf: Float32, displacement: Float32): Float32 =
    baseSdf + displacement

  /** Round: soften edges by subtracting radius. */
  def opRound(sdf: Float32, radius: Float32): Float32 =
    sdf - radius

  /** Onion: create a shell of given thickness. */
  def opOnion(sdf: Float32, thickness: Float32): Float32 =
    abs(sdf) - thickness
