package io.computenode.cyfra.samples.foton

import io.computenode.cyfra
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.expression.types.Float32.given
import io.computenode.cyfra.foton.library.GSeq
import io.computenode.cyfra.foton.library.Math3D.rotate
import io.computenode.cyfra.foton.library.Color.{InterpolationThemes, interpolate}
import io.computenode.cyfra.foton.animation.AnimatedFunctionRenderer.Parameters
import io.computenode.cyfra.foton.animation.AnimationFunctions.*
import io.computenode.cyfra.foton.animation.{AnimatedFunction, AnimatedFunctionRenderer}

import java.nio.file.Paths
import scala.concurrent.duration.DurationInt

object AnimatedJulia:
  @main
  def julia() =

    def julia(uv: Vec2[Float32])(using AnimationInstant): Int32 =
      val p = smooth(from = 0.355f, to = 0.4f, duration = 3.seconds)
      val const = Vec2(p, p)
      GSeq.gen(uv, next = v => Vec2((v.x * v.x) - (v.y * v.y), 2.0f * v.x * v.y) + const).limit(1000).map(v => length(v)).takeWhile(_ < 2.0f).count

    def juliaColor(uv: Vec2[Float32])(using AnimationInstant): Vec4[Float32] =
      val rotatedUv = rotate(uv, Math.PI.toFloat / 3.0f)
      val recursionCount = julia(rotatedUv)
      val f = min(Float32(1f), recursionCount.asFloat / 100f)
      val color = interpolate(InterpolationThemes.Blue, f)
      Vec4(color, 1.0f)

    val animatedJulia = AnimatedFunction.fromCoord(juliaColor, 3.seconds)

    val renderer = AnimatedFunctionRenderer(Parameters(1024, 1024, 30))
    renderer.renderFramesToDir(animatedJulia, Paths.get("julia"))
