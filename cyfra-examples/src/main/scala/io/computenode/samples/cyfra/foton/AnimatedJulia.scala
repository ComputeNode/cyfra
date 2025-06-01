package io.computenode.samples.cyfra.foton

import io.computenode.cyfra
import io.computenode.cyfra.*
import io.computenode.cyfra.foton.animation.AnimatedFunctionRenderer.Parameters
import io.computenode.cyfra.foton.animation.{AnimatedFunction, AnimatedFunctionRenderer}
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Color.{InterpolationThemes, interpolate}
import io.computenode.cyfra.dsl.Math3D.*
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.foton.animation.AnimationFunctions.*
import io.computenode.cyfra.runtime.SpirvOptimizer.{Enable, O}

import java.nio.file.Paths
import scala.concurrent.duration.DurationInt

object AnimatedJulia:
  given GContext = new GContext(spirvOptimization = Enable(O))
  @main
  def julia(): Unit =

    def julia(uv: Vec2[Float32])(using AnimationInstant): Int32 =
      val p = smooth(from = 0.355f, to = 0.4f, duration = 3.seconds)
      val const = (p, p)
      GSeq.gen(uv, next = v => {
        ((v.x * v.x) - (v.y * v.y), 2.0f * v.x * v.y) + const
      }).limit(1000).map(length).takeWhile(_ < 2.0f).count

    def juliaColor(uv: Vec2[Float32])(using AnimationInstant): Vec4[Float32] =
      val rotatedUv = rotate(uv, Math.PI.toFloat / 3.0f)
      val recursionCount = julia(rotatedUv)
      val f = min(1f, recursionCount.asFloat / 100f)
      val color = interpolate(InterpolationThemes.Blue, f)
      (
        color.r,
        color.g,
        color.b,
        1.0f
      )

    val animatedJulia = AnimatedFunction.fromCoord(juliaColor, 3.seconds)

    val renderer = AnimatedFunctionRenderer(Parameters(1024, 1024, 30))
    renderer.renderFramesToDir(animatedJulia, Paths.get("julia"))

