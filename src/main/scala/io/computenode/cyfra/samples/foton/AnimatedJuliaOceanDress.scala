package io.computenode.cyfra.samples.foton

import io.computenode.cyfra
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.GSeq
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.animation.AnimatedFunctionRenderer.Parameters
import io.computenode.cyfra.foton.animation.AnimationFunctions.*
import io.computenode.cyfra.foton.animation.{AnimatedFunction, AnimatedFunctionRenderer}
import io.computenode.cyfra.utility.Color.*
import io.computenode.cyfra.utility.Math3D.*

import scala.concurrent.duration.DurationInt
import java.nio.file.Paths

object AnimatedJuliaOcean:
  @main
  def juliaOcean() =

    def juliaOcean(uv: Vec2[Float32])(using AnimationInstant): Int32 =
      val p = smooth(from = 0.355f, to = 0.4f, duration = 3.seconds)
      val const = (p, p)
      GSeq.gen(uv, next = v => {
        ((v.x * v.x) - (v.y * v.y), 2.0f * v.x * v.y) + const
      }).limit(1000).map(length).takeWhile(_ < 2.0f).count

    def juliaOceanColor(uv: Vec2[Float32])(using AnimationInstant): Vec4[Float32] =
      val rotatedUv = rotate(uv, Math.PI.toFloat / 3.0f)
      val recursionCount = juliaOcean(rotatedUv)
      val f = min(1f, recursionCount.asFloat / 100f)
      val color = interpolate(InterpolationThemes.Ocean, f)
      (
        color.r,
        color.g,
        color.b,
        1.0f
      )

    val animatedJuliaOcean = AnimatedFunction.fromCoord(juliaOceanColor, 3.seconds) 

    val renderer = AnimatedFunctionRenderer(Parameters(1024, 1024, 30)) // Reduce resolution if our GPU is not capabale 
    renderer.renderFramesToDir(animatedJuliaOcean, Paths.get("juliaOcean"))
