package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import izumi.reflect.Tag

/**
 * Post-processing effects for SDF rendering.
 */
object PostProcessEffects:

  /** Fractional part (x - floor(x)), for positive values. */
  private def fract(x: Float32): Float32 = x - x.asInt.asFloat

  /**
   * Wang hash - excellent avalanche properties for integer hashing.
   * Used in Random.scala - proven to work well.
   */
  private def wangHash(seed: UInt32): UInt32 =
    val s1 = (seed ^ 61) ^ (seed >> 16)
    val s2 = s1 * 9
    val s3 = s2 ^ (s2 >> 4)
    val s4 = s3 * 0x27d4eb2d
    s4 ^ (s4 >> 15)

  /**
   * 3D hash for film grain - creates unique seed from pixel + frame.
   * Uses Wang hash for proper avalanche (no directional artifacts).
   */
  private def hash3d(px: Float32, py: Float32, frame: Float32): Float32 =
    // Combine coordinates into a unique integer seed
    // Use prime multipliers to avoid patterns
    val ix = px.asInt.unsigned
    val iy = py.asInt.unsigned
    val iframe = frame.asInt.unsigned
    // Combine with different primes for each dimension
    val seed = ix * 374761393 + iy * 668265263 + iframe * 1013904223
    val hashed = wangHash(seed)
    // Convert to float in [0, 1)
    hashed.asFloat / 4294967296.0f

  /** Compute luminance from RGB. */
  private def luminance(r: Float32, g: Float32, b: Float32): Float32 =
    r * 0.2126f + g * 0.7152f + b * 0.0722f

  /**
   * Dithering post-process to eliminate color banding.
   *
   * Uses interleaved gradient noise (IGN) which produces high-quality
   * screen-space noise without obvious repeating patterns. The noise
   * is triangular-distributed for optimal perceptual quality.
   *
   * Reference: Jorge Jimenez, "Next Generation Post Processing in Call of Duty: Advanced Warfare"
   *
   * @param ditherStrength Strength of dithering (default 1.0/255.0 for 8-bit output)
   */
  def dithering[C <: GStruct[C]: GStructSchema: Tag](
    runner: SdfRunner[C],
    ditherStrength: Float = 1.0f / 255.0f,
  )(
    getWidth: C => Int32,
    getTime: C => Float32,
  ): runner.PostProcess =
    GProgram.static[Int, runner.SdfLayout](
      layout = n => runner.SdfLayout(GBuffer[Vec4[Float16]](n), GUniform[C]()),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read
      val width = getWidth(cfg)
      val time = getTime(cfg)
      val totalPixels = runner.width * runner.height

      GIO.when(invocId < totalPixels):
        val px = invocId.mod(width)
        val py = invocId / width

        // Read current color (Vec4[Float16])
        val color = layout.pixels.read(invocId)
        val r = color.x.asFloat32
        val g = color.y.asFloat32
        val b = color.z.asFloat32
        val a = color.w.asFloat32

        // Interleaved Gradient Noise (IGN)
        // High-quality screen-space noise without obvious patterns
        val x = px.asFloat
        val y = py.asFloat

        // Base IGN formula: fract(52.9829189 * fract(0.06711056*x + 0.00583715*y))
        // Add time offset for temporal variation (reduces perception of noise)
        val timeOffset = time * 0.1f
        val dot = 0.06711056f * x + 0.00583715f * y + timeOffset
        val frac1 = fract(dot)
        val ign = 52.9829189f * frac1
        val noise = fract(ign)

        // Convert to triangular distribution for better perceptual quality
        // Triangular noise: subtract 0.5 and map to [-strength, +strength]
        val triangleNoise = (noise - 0.5f) * 2.0f * ditherStrength

        // Apply dither to RGB channels (not alpha)
        val newR = r + triangleNoise
        val newG = g + triangleNoise
        val newB = b + triangleNoise

        // Write back using Pixels helper
        layout.pixels.write(invocId, Pixels.rgba16f(newR, newG, newB, a))

  /**
   * Film grain effect that simulates analog film texture.
   *
   * The grain intensity is modulated by luminance - visible in midtones,
   * less visible in deep shadows and bright highlights (like real film).
   * Grain is temporal (changes each frame) for authentic motion-picture look.
   *
   * @param grainStrength Overall intensity of grain (0.03-0.08 for subtle, 0.1-0.2 for heavy)
   * @param grainSize     Size multiplier for grain particles (1.0 = pixel-sized)
   */
  def filmGrain[C <: GStruct[C]: GStructSchema: Tag](
    runner: SdfRunner[C],
    grainStrength: Float = 0.05f,
    grainSize: Float = 1.0f,
  )(
    getWidth: C => Int32,
    getHeight: C => Int32,
    getTime: C => Float32,
  ): runner.PostProcess =
    GProgram.static[Int, runner.SdfLayout](
      layout = n => runner.SdfLayout(GBuffer[Vec4[Float16]](n), GUniform[C]()),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read
      val width = getWidth(cfg)
      val height = getHeight(cfg)
      val time = getTime(cfg)
      val totalPixels = runner.width * runner.height

      GIO.when(invocId < totalPixels):
        val px = invocId.mod(width)
        val py = invocId / width

        val color = layout.pixels.read(invocId)
        val r = color.x.asFloat32
        val g = color.y.asFloat32
        val b = color.z.asFloat32
        val a = color.w.asFloat32

        // Calculate grain coordinates (scaled by grain size)
        val grainX = px.asFloat / grainSize
        val grainY = py.asFloat / grainSize

        // Frame number - completely new random pattern each frame (no scrolling)
        val frame = (time * 24.0f).asInt.asFloat // ~24fps grain refresh

        // Multi-octave noise for more organic grain texture
        // Frame is a separate parameter that scrambles output (not shifts pattern)
        val noise1 = hash3d(grainX, grainY, frame)
        val noise2 = hash3d(grainX * 2.1f + 100.0f, grainY * 2.1f + 50.0f, frame + 0.5f)
        val combinedNoise = noise1 * 0.7f + noise2 * 0.3f

        // Convert to centered grain value [-1, 1]
        val grain = (combinedNoise - 0.5f) * 2.0f

        // Luminance-dependent grain intensity (more visible in midtones)
        // Uses a bell curve centered around 0.5 luminance
        val lum = luminance(r, g, b)
        val lumFactor = 4.0f * lum * (1.0f - lum) // Peaks at lum=0.5
        val finalGrain = grain * grainStrength * (0.5f + lumFactor * 0.5f)

        // Apply grain to RGB
        val newR = clamp(r + finalGrain, 0.0f, 1.0f)
        val newG = clamp(g + finalGrain, 0.0f, 1.0f)
        val newB = clamp(b + finalGrain, 0.0f, 1.0f)

        layout.pixels.write(invocId, Pixels.rgba16f(newR, newG, newB, a))

  /**
   * Cinematic color grading with vignette, contrast, and split-toning.
   *
   * Implements a professional color grading pipeline:
   * 1. Vignette - darkens edges for cinematic framing
   * 2. Contrast - S-curve for punch and drama
   * 3. Saturation - adjustable color intensity
   * 4. Split-toning - adds complementary colors to shadows/highlights
   *    (default: teal shadows, warm highlights - classic cinema look)
   * 5. Lift/Gamma/Gain - fine-tune shadows, midtones, highlights
   *
   * @param vignetteStrength How much to darken edges (0.0 = none, 0.5 = subtle, 1.0 = heavy)
   * @param vignetteSoftness Falloff of vignette (1.0 = soft, 2.0+ = harder edge)
   * @param contrast         Contrast adjustment (1.0 = neutral, 1.2 = punchy)
   * @param saturation       Saturation (1.0 = neutral, 0.8 = slightly desaturated)
   * @param shadowTint       RGB tint for shadows (e.g., teal = (0.0, 0.05, 0.08))
   * @param highlightTint    RGB tint for highlights (e.g., warm = (0.05, 0.02, 0.0))
   * @param lift             Shadows adjustment RGB (-0.1 to 0.1)
   * @param gamma            Midtones adjustment RGB (0.9 to 1.1)
   * @param gain             Highlights adjustment RGB (0.9 to 1.1)
   */
  def cinematicGrade[C <: GStruct[C]: GStructSchema: Tag](
    runner: SdfRunner[C],
    vignetteStrength: Float = 0.4f,
    vignetteSoftness: Float = 1.5f,
    contrast: Float = 1.1f,
    saturation: Float = 0.95f,
    shadowTint: (Float, Float, Float) = (0.0f, 0.03f, 0.06f),   // Teal shadows
    highlightTint: (Float, Float, Float) = (0.04f, 0.02f, 0.0f), // Warm highlights
    lift: (Float, Float, Float) = (0.0f, 0.0f, 0.0f),
    gamma: (Float, Float, Float) = (1.0f, 1.0f, 1.0f),
    gain: (Float, Float, Float) = (1.0f, 1.0f, 1.0f),
  )(
    getWidth: C => Int32,
    getHeight: C => Int32,
  ): runner.PostProcess =
    GProgram.static[Int, runner.SdfLayout](
      layout = n => runner.SdfLayout(GBuffer[Vec4[Float16]](n), GUniform[C]()),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read
      val width = getWidth(cfg)
      val height = getHeight(cfg)
      val totalPixels = runner.width * runner.height

      GIO.when(invocId < totalPixels):
        val px = invocId.mod(width)
        val py = invocId / width

        val color = layout.pixels.read(invocId)
        var r = color.x.asFloat32
        var g = color.y.asFloat32
        var b = color.z.asFloat32
        val a = color.w.asFloat32

        // 1. Vignette - darken edges
        val uvX = px.asFloat / width.asFloat
        val uvY = py.asFloat / height.asFloat
        val centeredX = uvX - 0.5f
        val centeredY = uvY - 0.5f
        // Elliptical distance from center (adjusted for aspect ratio)
        val aspect = width.asFloat / height.asFloat
        val distSq = centeredX * centeredX * aspect * aspect + centeredY * centeredY
        val dist = sqrt(distSq)
        // Smooth vignette falloff
        val vignette = 1.0f - clamp(dist * vignetteSoftness, 0.0f, 1.0f) * vignetteStrength
        r = r * vignette
        g = g * vignette
        b = b * vignette

        // 2. Contrast (S-curve via simple power function)
        // Move to 0-1 range, apply curve, move back
        val contrastPivot = 0.5f
        r = contrastPivot + (r - contrastPivot) * contrast
        g = contrastPivot + (g - contrastPivot) * contrast
        b = contrastPivot + (b - contrastPivot) * contrast

        // 3. Saturation adjustment
        val lum = luminance(r, g, b)
        r = lum + (r - lum) * saturation
        g = lum + (g - lum) * saturation
        b = lum + (b - lum) * saturation

        // 4. Split-toning (tint shadows and highlights differently)
        // Shadow amount: inverse of luminance
        val shadowAmount = 1.0f - clamp(lum * 2.0f, 0.0f, 1.0f)
        // Highlight amount: luminance above 0.5
        val highlightAmount = clamp((lum - 0.5f) * 2.0f, 0.0f, 1.0f)

        r = r + shadowTint._1 * shadowAmount + highlightTint._1 * highlightAmount
        g = g + shadowTint._2 * shadowAmount + highlightTint._2 * highlightAmount
        b = b + shadowTint._3 * shadowAmount + highlightTint._3 * highlightAmount

        // 5. Lift/Gamma/Gain (professional color correction)
        // Lift: adds to shadows
        r = r + lift._1 * (1.0f - r)
        g = g + lift._2 * (1.0f - g)
        b = b + lift._3 * (1.0f - b)

        // Gamma: power function for midtones (inverted, so >1 = brighter mids)
        val gammaR = when(r > 0.0f)(pow(r, 1.0f / gamma._1)).otherwise(0.0f)
        val gammaG = when(g > 0.0f)(pow(g, 1.0f / gamma._2)).otherwise(0.0f)
        val gammaB = when(b > 0.0f)(pow(b, 1.0f / gamma._3)).otherwise(0.0f)

        // Gain: multiplier for highlights
        val finalR = clamp(gammaR * gain._1, 0.0f, 1.0f)
        val finalG = clamp(gammaG * gain._2, 0.0f, 1.0f)
        val finalB = clamp(gammaB * gain._3, 0.0f, 1.0f)

        layout.pixels.write(invocId, Pixels.rgba16f(finalR, finalG, finalB, a))

  /**
   * Combined film look: grain + cinematic grading + dithering in one pass.
   * More efficient than chaining separate effects.
   */
  def filmLook[C <: GStruct[C]: GStructSchema: Tag](
    runner: SdfRunner[C],
    grainStrength: Float = 0.04f,
    vignetteStrength: Float = 0.35f,
    contrast: Float = 1.08f,
    saturation: Float = 0.92f,
  )(
    getWidth: C => Int32,
    getHeight: C => Int32,
    getTime: C => Float32,
  ): runner.PostProcess =
    GProgram.static[Int, runner.SdfLayout](
      layout = n => runner.SdfLayout(GBuffer[Vec4[Float16]](n), GUniform[C]()),
      dispatchSize = identity,
    ): layout =>
      val invocId = GIO.invocationId
      val cfg = layout.config.read
      val width = getWidth(cfg)
      val height = getHeight(cfg)
      val time = getTime(cfg)
      val totalPixels = runner.width * runner.height

      GIO.when(invocId < totalPixels):
        val px = invocId.mod(width)
        val py = invocId / width

        val color = layout.pixels.read(invocId)
        var r = color.x.asFloat32
        var g = color.y.asFloat32
        var b = color.z.asFloat32
        val a = color.w.asFloat32

        // --- Vignette ---
        val uvX = px.asFloat / width.asFloat
        val uvY = py.asFloat / height.asFloat
        val centeredX = uvX - 0.5f
        val centeredY = uvY - 0.5f
        val aspect = width.asFloat / height.asFloat
        val distSq = centeredX * centeredX * aspect * aspect + centeredY * centeredY
        val vignette = 1.0f - clamp(sqrt(distSq) * 1.4f, 0.0f, 1.0f) * vignetteStrength
        r = r * vignette
        g = g * vignette
        b = b * vignette

        // --- Contrast ---
        r = 0.5f + (r - 0.5f) * contrast
        g = 0.5f + (g - 0.5f) * contrast
        b = 0.5f + (b - 0.5f) * contrast

        // --- Saturation ---
        val lum = luminance(r, g, b)
        r = lum + (r - lum) * saturation
        g = lum + (g - lum) * saturation
        b = lum + (b - lum) * saturation

        // --- Split toning (subtle teal shadows, warm highlights) ---
        val shadowAmount = 1.0f - clamp(lum * 2.0f, 0.0f, 1.0f)
        val highlightAmount = clamp((lum - 0.5f) * 2.0f, 0.0f, 1.0f)
        r = r + 0.0f * shadowAmount + 0.03f * highlightAmount
        g = g + 0.025f * shadowAmount + 0.015f * highlightAmount
        b = b + 0.05f * shadowAmount + 0.0f * highlightAmount

        // --- Film grain (frame as separate seed, no scrolling) ---
        val frame = (time * 24.0f).asInt.asFloat
        val noise1 = hash3d(px.asFloat, py.asFloat, frame)
        val noise2 = hash3d(px.asFloat * 2.1f + 100.0f, py.asFloat * 2.1f + 50.0f, frame + 0.5f)
        val grain = ((noise1 * 0.7f + noise2 * 0.3f) - 0.5f) * 2.0f
        val lumFactor = 4.0f * lum * (1.0f - lum)
        val finalGrain = grain * grainStrength * (0.5f + lumFactor * 0.5f)
        r = r + finalGrain
        g = g + finalGrain
        b = b + finalGrain

        // --- Dithering (always last) ---
        val timeOffset = time * 0.1f
        val dot = 0.06711056f * px.asFloat + 0.00583715f * py.asFloat + timeOffset
        val ign = 52.9829189f * fract(dot)
        val dither = (fract(ign) - 0.5f) * (2.0f / 255.0f)
        r = r + dither
        g = g + dither
        b = b + dither

        // Clamp and write
        layout.pixels.write(invocId, Pixels.rgba16f(
          clamp(r, 0.0f, 1.0f),
          clamp(g, 0.0f, 1.0f),
          clamp(b, 0.0f, 1.0f),
          a,
        ))
