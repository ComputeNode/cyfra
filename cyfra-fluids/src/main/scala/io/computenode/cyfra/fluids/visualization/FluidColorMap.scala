package io.computenode.cyfra.fluids.visualization

import io.computenode.cyfra.dsl.{*, given}

/** Utility functions for mapping fluid properties to colors. */
object FluidColorMap:

  /** Simple grayscale mapping based on density.
    *
    * @param density
    *   Fluid density value (0.0 to 1.0)
    * @return
    *   RGBA color with density as grayscale
    */
  def grayscale(density: Float32)(using Source): Vec4[Float32] =
    val clamped = clamp(density, 0.0f, 1.0f)
    vec4(clamped, clamped, clamped, clamped)

  /** Temperature-based color ramp (cold blue to hot red).
    *
    * Maps density to a heat map:
    *   - 0.0 = black
    *   - 0.2 = dark blue
    *   - 0.4 = cyan
    *   - 0.6 = yellow
    *   - 0.8 = orange
    *   - 1.0 = white
    *
    * @param density
    *   Fluid density value
    * @return
    *   RGBA color on the heat map
    */
  def heatMap(density: Float32)(using Source): Vec4[Float32] =
    val d = clamp(density, 0.0f, 1.0f)

    // Piecewise linear color ramp
    val r = when(d < 0.5f)(d * 2.0f).otherwise(1.0f)
    val g = when(d < 0.25f)(0.0f).otherwise(when(d < 0.75f)((d - 0.25f) * 2.0f).otherwise(1.0f))
    val b = when(d < 0.5f)(1.0f - d * 2.0f).otherwise(0.0f)

    vec4(r, g, b, 1.0f)

  /** Smoke-like appearance with soft falloff.
    *
    * Creates a smoky white appearance with soft edges.
    *
    * @param density
    *   Fluid density value
    * @return
    *   RGBA color with alpha for blending
    */
  def smoke(density: Float32)(using Source): Vec4[Float32] =
    val d = clamp(density, 0.0f, 1.0f)
    val brightness = sqrt(d) // Gamma correction for softer appearance
    val alpha = d * 0.8f // Not fully opaque
    vec4(brightness, brightness, brightness, 1.0f)

  /** Velocity magnitude-based coloring.
    *
    * Colors based on velocity magnitude:
    *   - Slow = blue/cold
    *   - Fast = red/hot
    *
    * @param velocity
    *   Velocity vector
    * @param maxSpeed
    *   Maximum expected speed for normalization
    * @return
    *   RGBA color representing speed
    */
  def velocityMagnitude(velocity: Vec4[Float32], maxSpeed: Float32)(using Source): Vec4[Float32] =
    val vel3 = vec3(velocity.x, velocity.y, velocity.z)
    val speed = sqrt(vel3 dot vel3)
    val normalized = clamp(speed / maxSpeed, 0.0f, 1.0f)
    heatMap(normalized)
