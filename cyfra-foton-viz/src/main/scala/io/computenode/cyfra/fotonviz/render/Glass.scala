package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}

/**
 * Glass material definition.
 *
 * All raymarching and optical calculations are in [[Raymarch]].
 * This module provides material presets and helper shading functions.
 */
object Glass:

  /**
   * Glass material properties.
   *
   * @param ior             Index of refraction (1.0=air, 1.33=water, 1.5=glass, 2.4=diamond)
   * @param opacity         Base opacity for diffuse contribution (0=pure glass, 1=opaque)
   * @param tint            Color tint for Beer's law absorption
   * @param absorption      Absorption strength (higher = more colored at depth)
   * @param scatterStrength Subsurface scattering intensity
   * @param scatterColor    Color of subsurface scattering glow
   */
  case class Material(
    ior: Float,
    opacity: Float,
    tint: (Float, Float, Float),
    absorption: Float,
    scatterStrength: Float,
    scatterColor: (Float, Float, Float),
  ):
    def tintVec: Vec3[Float32] = vec3(tint._1, tint._2, tint._3)
    def scatterVec: Vec3[Float32] = vec3(scatterColor._1, scatterColor._2, scatterColor._3)

  // ---- Presets ----

  /** Clear glass with slight blue tint. */
  val Clear: Material = Material(
    ior = 1.5f,
    opacity = 0.08f,
    tint = (0.98f, 0.99f, 1.0f),
    absorption = 0.3f,
    scatterStrength = 0.15f,
    scatterColor = (0.9f, 0.95f, 1.0f),
  )

  /** Blue tinted glass. */
  val Blue: Material = Material(
    ior = 1.5f,
    opacity = 0.1f,
    tint = (0.7f, 0.85f, 1.0f),
    absorption = 0.8f,
    scatterStrength = 0.25f,
    scatterColor = (0.6f, 0.8f, 1.0f),
  )

  /** Amber/orange glass. */
  val Amber: Material = Material(
    ior = 1.5f,
    opacity = 0.12f,
    tint = (1.0f, 0.85f, 0.6f),
    absorption = 0.7f,
    scatterStrength = 0.2f,
    scatterColor = (1.0f, 0.9f, 0.7f),
  )

  /** Green bottle glass. */
  val Green: Material = Material(
    ior = 1.52f,
    opacity = 0.15f,
    tint = (0.7f, 1.0f, 0.75f),
    absorption = 0.9f,
    scatterStrength = 0.18f,
    scatterColor = (0.8f, 1.0f, 0.85f),
  )

  /** Ruby red glass. */
  val Ruby: Material = Material(
    ior = 1.76f,
    opacity = 0.2f,
    tint = (1.0f, 0.3f, 0.4f),
    absorption = 1.2f,
    scatterStrength = 0.3f,
    scatterColor = (1.0f, 0.5f, 0.6f),
  )

  /** Diamond-like material. */
  val Diamond: Material = Material(
    ior = 2.42f,
    opacity = 0.02f,
    tint = (0.98f, 0.98f, 1.0f),
    absorption = 0.1f,
    scatterStrength = 0.05f,
    scatterColor = (1.0f, 1.0f, 1.0f),
  )

  /** Water. */
  val Water: Material = Material(
    ior = 1.33f,
    opacity = 0.05f,
    tint = (0.9f, 0.95f, 1.0f),
    absorption = 0.2f,
    scatterStrength = 0.1f,
    scatterColor = (0.85f, 0.92f, 1.0f),
  )

  /** Ice. */
  val Ice: Material = Material(
    ior = 1.31f,
    opacity = 0.1f,
    tint = (0.92f, 0.97f, 1.0f),
    absorption = 0.25f,
    scatterStrength = 0.3f,
    scatterColor = (0.9f, 0.95f, 1.0f),
  )
