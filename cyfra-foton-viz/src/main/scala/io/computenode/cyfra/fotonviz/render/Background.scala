package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}

/** Background and environment effects. */
object Background:

  /** Simple hash function for noise generation. */
  def hash(x: Float32, y: Float32): Float32 =
    val n = (x * 374761.0f + y * 668265.0f).asInt
    val m = (n ^ (n >> 13)) * 1274126177
    ((m ^ (m >> 16)) & 0xFFFF).asFloat / 65535.0f

  /** Multi-octave noise. */
  def noise2(x: Float32, y: Float32, octaves: Int = 2): Float32 =
    val n1 = hash(x, y)
    val n2 = hash(x * 2.0f + 100.0f, y * 2.0f + 100.0f)
    (n1 * 0.6f + n2 * 0.4f)

  /**
   * Vignette with noise - darkens edges with organic texture.
   *
   * @param u        Normalized x coordinate [-1, 1]
   * @param v        Normalized y coordinate [-1, 1]
   * @param px       Pixel x (for noise)
   * @param py       Pixel y (for noise)
   * @param strength Overall vignette darkness
   * @return Background color (RGB)
   */
  def vignette(
    u: Float32,
    v: Float32,
    px: Float32,
    py: Float32,
    strength: Float32 = 0.35f,
  ): Vec3[Float32] =
    val d = sqrt(u * u + v * v)
    val base = clamp(d * 0.5f, 0.0f, 1.0f)
    val falloff = base * base * base

    val noise1 = hash(px * 0.1f, py * 0.1f)
    val noise2 = hash(px * 0.05f + 100.0f, py * 0.05f + 100.0f)
    val noise = (noise1 * 0.6f + noise2 * 0.4f) * 0.15f

    val darkening = falloff * strength + noise * falloff
    vec3(1.0f - darkening, 1.0f - darkening, 1.0f - darkening * 0.9f)

  /** Solid color background. */
  def solid(r: Float32, g: Float32, b: Float32): Vec3[Float32] = vec3(r, g, b)

  /** Gradient from top to bottom color. */
  def verticalGradient(
    v: Float32,
    topColor: Vec3[Float32],
    bottomColor: Vec3[Float32],
  ): Vec3[Float32] =
    val t = (v + 1.0f) * 0.5f  // Map [-1, 1] to [0, 1]
    vec3(
      bottomColor.x * (1.0f - t) + topColor.x * t,
      bottomColor.y * (1.0f - t) + topColor.y * t,
      bottomColor.z * (1.0f - t) + topColor.z * t,
    )
