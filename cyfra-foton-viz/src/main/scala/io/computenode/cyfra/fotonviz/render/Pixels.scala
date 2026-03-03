package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.Expression.ComposeVec4
import io.computenode.cyfra.dsl.macros.Source

/** Pixel color utilities for RGBA16F format. */
object Pixels:

  /** Construct Vec4[Float16] from 4 Float16 values. */
  private def vec4f16(r: Float16, g: Float16, b: Float16, a: Float16)(using Source): Vec4[Float16] =
    Vec4(ComposeVec4[Float16](r, g, b, a))

  /** Create RGBA16F pixel from RGB floats (alpha = 1). */
  def rgba16f(r: Float32, g: Float32, b: Float32)(using Source): Vec4[Float16] =
    vec4f16(r.asFloat16, g.asFloat16, b.asFloat16, (1.0f: Float32).asFloat16)

  /** Create RGBA16F pixel from RGBA floats. */
  def rgba16f(r: Float32, g: Float32, b: Float32, a: Float32)(using Source): Vec4[Float16] =
    vec4f16(r.asFloat16, g.asFloat16, b.asFloat16, a.asFloat16)

  /** Create RGBA16F pixel from Vec3 color (alpha = 1). */
  def rgba16f(color: Vec3[Float32])(using Source): Vec4[Float16] =
    vec4f16(color.x.asFloat16, color.y.asFloat16, color.z.asFloat16, (1.0f: Float32).asFloat16)

  /** Create RGBA16F pixel from Vec4 color. */
  def rgba16f(color: Vec4[Float32])(using Source): Vec4[Float16] =
    vec4f16(color.x.asFloat16, color.y.asFloat16, color.z.asFloat16, color.w.asFloat16)
