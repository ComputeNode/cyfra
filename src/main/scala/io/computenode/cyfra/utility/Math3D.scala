package io.computenode.cyfra.utility

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.{*, given}

import scala.concurrent.duration.DurationInt

object Math3D:
  def scalarTriple(u: Vec3[Float32], v: Vec3[Float32], w: Vec3[Float32]): Float32 = (u cross v) dot w

  def fresnelReflectAmount(
    n1: Float32,
    n2: Float32,
    normal: Vec3[Float32],
    incident: Vec3[Float32],
    f0: Float32,
    f90: Float32
  ): Float32 =
    val r0 = ((n1 - n2) / (n1 + n2)) * ((n1 - n2) / (n1 + n2))
    val cosX = -(normal dot incident)
    when(n1 > n2) {
      val n = n1 / n2
      val sinT2 = n * n * (1f - cosX * cosX)
      when(sinT2 > 1f) {
        f90
      } otherwise {
        val cosX2 = sqrt(1.0f - sinT2)
        val x = 1.0f - cosX2
        val ret = r0 + ((1.0f - r0) * x * x * x * x * x)
        mix(f0, f90, ret)
      }
    } otherwise {
      val x = 1.0f - cosX
      val ret = r0 + ((1.0f - r0) * x * x * x * x * x)
      mix(f0, f90, ret)
    }

  def lessThan(f: Vec3[Float32], f2: Float32): Vec3[Float32] =
    (when(f.x < f2)(1.0f).otherwise(0.0f),
      when(f.y < f2)(1.0f).otherwise(0.0f),
      when(f.z < f2)(1.0f).otherwise(0.0f))

  def rotate(uv: Vec2[Float32], angle: Float32): Vec2[Float32] =
    val newXAxis = (cos(angle), sin(angle))
    val newYAxis = (-newXAxis.y, newXAxis.x)
    (uv dot newXAxis, uv dot newYAxis) * 0.9f

  def rotate(angle: Float32)(uv: Vec2[Float32]): Vec2[Float32] =
    rotate(uv, angle)
