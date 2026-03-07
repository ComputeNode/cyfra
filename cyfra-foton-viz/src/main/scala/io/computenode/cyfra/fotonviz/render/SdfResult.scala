package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}

/**
 * SDF evaluation result with material information.
 *
 * @param distance  Signed distance to the nearest surface
 * @param materials Packed material intensities (4 × 8-bit values)
 *                  - Bits 0-7:   Material 0 intensity (0-255)
 *                  - Bits 8-15:  Material 1 intensity
 *                  - Bits 16-23: Material 2 intensity
 *                  - Bits 24-31: Material 3 intensity
 */
case class SdfResult(
  distance: Float32,
  materials: UInt32,
) extends GStruct[SdfResult]

object SdfResult:
  given GStructSchema[SdfResult] = GStructSchema.derived

  /** Create result with single material at full intensity. */
  def apply(distance: Float32, materialId: Int32)(using io.computenode.cyfra.dsl.macros.Source): SdfResult =
    val shift = materialId * 8
    val packed = (255: UInt32) << shift.unsigned
    SdfResult(distance, packed)

  /** Create result with explicit material intensities (0-255 each). */
  def apply(distance: Float32, m0: UInt32, m1: UInt32, m2: UInt32, m3: UInt32)(using io.computenode.cyfra.dsl.macros.Source): SdfResult =
    val packed = (m0 & 0xFF.unsigned) |
      ((m1 & 0xFF.unsigned) << 8) |
      ((m2 & 0xFF.unsigned) << 16) |
      ((m3 & 0xFF.unsigned) << 24)
    SdfResult(distance, packed)

  // ---- Material Intensity Extraction ----

  /** Extract intensity for material 0 (0-255). */
  def intensity0(materials: UInt32): UInt32 = materials & 0xFF.unsigned

  /** Extract intensity for material 1 (0-255). */
  def intensity1(materials: UInt32): UInt32 = (materials >> 8) & 0xFF.unsigned

  /** Extract intensity for material 2 (0-255). */
  def intensity2(materials: UInt32): UInt32 = (materials >> 16) & 0xFF.unsigned

  /** Extract intensity for material 3 (0-255). */
  def intensity3(materials: UInt32): UInt32 = (materials >> 24) & 0xFF.unsigned

  /** Extract intensity for material by ID (0-3). */
  def intensity(materials: UInt32, id: Int32): UInt32 =
    val shift = id.unsigned * 8.unsigned
    (materials >> shift) & 0xFF.unsigned

  /** Extract normalized intensity (0.0 - 1.0) for material by ID. */
  def intensityNormalized(materials: UInt32, id: Int32): Float32 =
    intensity(materials, id).asFloat / 255.0f

  // ---- Material Blending ----

  /**
   * Blend two material packs based on interpolation factor.
   *
   * @param a First material pack
   * @param b Second material pack
   * @param t Blend factor (0.0 = all a, 1.0 = all b)
   */
  def blendMaterials(a: UInt32, b: UInt32, t: Float32): UInt32 =
    val tClamped = clamp(t, 0.0f, 1.0f)
    val oneMinusT = 1.0f - tClamped

    val a0 = (a & 0xFF.unsigned).asFloat
    val a1 = ((a >> 8) & 0xFF.unsigned).asFloat
    val a2 = ((a >> 16) & 0xFF.unsigned).asFloat
    val a3 = ((a >> 24) & 0xFF.unsigned).asFloat

    val b0 = (b & 0xFF.unsigned).asFloat
    val b1 = ((b >> 8) & 0xFF.unsigned).asFloat
    val b2 = ((b >> 16) & 0xFF.unsigned).asFloat
    val b3 = ((b >> 24) & 0xFF.unsigned).asFloat

    val r0 = (a0 * oneMinusT + b0 * tClamped).asInt.unsigned & 0xFF.unsigned
    val r1 = (a1 * oneMinusT + b1 * tClamped).asInt.unsigned & 0xFF.unsigned
    val r2 = (a2 * oneMinusT + b2 * tClamped).asInt.unsigned & 0xFF.unsigned
    val r3 = (a3 * oneMinusT + b3 * tClamped).asInt.unsigned & 0xFF.unsigned

    r0 | (r1 << 8) | (r2 << 16) | (r3 << 24)
