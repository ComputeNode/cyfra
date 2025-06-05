package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Functions.{cos, mix, pow}
import io.computenode.cyfra.dsl.Value.{Float32, Vec3}
import io.computenode.cyfra.dsl.Math3D.lessThan

import scala.annotation.targetName

object Color:

  def SRGBToLinear(rgb: Vec3[Float32]): Vec3[Float32] = {
    val clampedRgb = vclamp(rgb, 0.0f, 1.0f)
    mix(pow((clampedRgb + vec3(0.055f)) * (1.0f / 1.055f), vec3(2.4f)), clampedRgb * (1.0f / 12.92f), lessThan(clampedRgb, 0.04045f))
  }

  // https://www.youtube.com/shorts/TH3OTy5fTog
  def igPallette(brightness: Vec3[Float32], contrast: Vec3[Float32], freq: Vec3[Float32], offsets: Vec3[Float32], f: Float32): Vec3[Float32] =
    brightness addV (contrast mulV cos(((freq * f) addV offsets) * 2f * math.Pi.toFloat))

  def linearToSRGB(rgb: Vec3[Float32]): Vec3[Float32] = {
    val clampedRgb = vclamp(rgb, 0.0f, 1.0f)
    mix(pow(clampedRgb, vec3(1.0f / 2.4f)) * 1.055f - vec3(0.055f), clampedRgb * 12.92f, lessThan(clampedRgb, 0.0031308f))
  }

  type InterpolationTheme = (Vec3[Float32], Vec3[Float32], Vec3[Float32])
  object InterpolationThemes:
    val Blue: InterpolationTheme = ((8f, 22f, 104f) * (1 / 255f), (62f, 82f, 199f) * (1 / 255f), (221f, 233f, 255f) * (1 / 255f))
    val Black: InterpolationTheme = ((255f, 255f, 255f) * (1 / 255f), (0f, 0f, 0f), (0f, 0f, 0f))

  def interpolate(theme: InterpolationTheme, f: Float32): Vec3[Float32] =
    val (c1, c2, c3) = theme
    val ratio1 = (1f - f) * (1f - f)
    val ratio2 = 2f * f * (1f - f)
    val ratio3 = f * f
    c1 * ratio1 + c2 * ratio2 + c3 * ratio3

  @targetName("interpolatePiped")
  def interpolate(theme: InterpolationTheme)(f: Float32): Vec3[Float32] = interpolate(theme, f)

  transparent inline def hex(inline color: String): Any = ${ hexImpl('{ color }) }

  import scala.quoted.*
  def hexImpl(color: Expr[String])(using Quotes): Expr[Any] =
    val str = color.valueOrAbort
    val rgbPattern = """#?([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})""".r
    val rgbaPattern = """#?([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})""".r
    def byteHexToFloat(hex: String): Float = Integer.parseInt(hex, 16) / 255f
    def byteHexToFloatExpr(hex: String): Expr[Float] = Expr(byteHexToFloat(hex))
    str match
      case rgbPattern(r, g, b) => '{ (${ byteHexToFloatExpr(r) }, ${ byteHexToFloatExpr(g) }, ${ byteHexToFloatExpr(b) }) }
      case rgbaPattern(r, g, b, a) =>
        '{ (${ byteHexToFloatExpr(r) }, ${ byteHexToFloatExpr(g) }, ${ byteHexToFloatExpr(b) }, ${ byteHexToFloatExpr(a) }) }
      case _ => quotes.reflect.report.errorAndAbort(s"Invalid color format: $str")
