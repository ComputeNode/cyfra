package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}

/**
 * Material properties for shading.
 *
 * Stored as a GStruct so it can be passed to GPU.
 * Up to 4 materials supported per scene (indexed 0-3).
 *
 * @param isTransparent 1.0 if transparent (glass-like), 0.0 if solid
 * @param ior           Index of refraction (only for transparent materials)
 * @param dispersion    Chromatic dispersion strength (rainbow effect)
 * @param absorption    Light absorption coefficient for Beer's law
 * @param colorR        Base color red component
 * @param colorG        Base color green component
 * @param colorB        Base color blue component
 * @param roughness     Surface roughness (0 = mirror, 1 = diffuse)
 * @param metallic      Metallic factor (affects Fresnel and color)
 */
case class MaterialProps(
  isTransparent: Float32,
  ior: Float32,
  dispersion: Float32,
  absorption: Float32,
  colorR: Float32,
  colorG: Float32,
  colorB: Float32,
  roughness: Float32,
  metallic: Float32,
) extends GStruct[MaterialProps]

object MaterialProps:
  given GStructSchema[MaterialProps] = GStructSchema.derived

/**
 * Material pack containing up to 4 materials for a scene.
 */
case class MaterialPack(
  mat0: MaterialProps,
  mat1: MaterialProps,
  mat2: MaterialProps,
  mat3: MaterialProps,
) extends GStruct[MaterialPack]

object MaterialPack:
  given GStructSchema[MaterialPack] = GStructSchema.derived

/**
 * CPU-side material definition for easy construction.
 */
object Material:

  case class Def(
    isTransparent: Boolean,
    ior: Float = 1.0f,
    dispersion: Float = 0.0f,
    absorption: Float = 0.0f,
    color: (Float, Float, Float) = (1f, 1f, 1f),
    roughness: Float = 0.5f,
    metallic: Float = 0.0f,
  ):
    def toProps: MaterialProps = MaterialProps(
      isTransparent = if isTransparent then 1.0f else 0.0f,
      ior = ior,
      dispersion = dispersion,
      absorption = absorption,
      colorR = color._1,
      colorG = color._2,
      colorB = color._3,
      roughness = roughness,
      metallic = metallic,
    )

  // ---- Presets ----

  /** Default solid white material. */
  val Solid: Def = Def(
    isTransparent = false,
    color = (0.9f, 0.9f, 0.9f),
    roughness = 0.3f,
  )

  /** Metallic material. */
  def metal(color: (Float, Float, Float), roughness: Float = 0.2f): Def = Def(
    isTransparent = false,
    color = color,
    roughness = roughness,
    metallic = 1.0f,
  )

  /** Clear glass. */
  val Glass: Def = Def(
    isTransparent = true,
    ior = 1.5f,
    dispersion = 0.04f,
    absorption = 0.3f,
    color = (0.98f, 0.99f, 1.0f),
    roughness = 0.0f,
  )

  /** Colored glass. */
  def glass(
    color: (Float, Float, Float),
    ior: Float = 1.5f,
    dispersion: Float = 0.04f,
    absorption: Float = 0.5f,
  ): Def = Def(
    isTransparent = true,
    ior = ior,
    dispersion = dispersion,
    absorption = absorption,
    color = color,
    roughness = 0.0f,
  )

  /** Diamond. */
  val Diamond: Def = Def(
    isTransparent = true,
    ior = 2.42f,
    dispersion = 0.044f,
    absorption = 0.1f,
    color = (1.0f, 1.0f, 1.0f),
    roughness = 0.0f,
  )

  /** Water. */
  val Water: Def = Def(
    isTransparent = true,
    ior = 1.33f,
    dispersion = 0.02f,
    absorption = 0.2f,
    color = (0.9f, 0.95f, 1.0f),
    roughness = 0.0f,
  )

  /** Empty/unused material slot. */
  val Empty: Def = Def(
    isTransparent = false,
    color = (0f, 0f, 0f),
  )

  /** Create a MaterialPack from up to 4 material definitions. */
  def pack(
    m0: Def = Empty,
    m1: Def = Empty,
    m2: Def = Empty,
    m3: Def = Empty,
  ): MaterialPack = MaterialPack(
    mat0 = m0.toProps,
    mat1 = m1.toProps,
    mat2 = m2.toProps,
    mat3 = m3.toProps,
  )
