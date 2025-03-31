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
import io.computenode.cyfra.dsl.Control.when

object AnimatedConnectingForms:
  @main
  def AnimatedConnectingForms2() =

    def connectingForms(uv: Vec2[Float32])(using AnimationInstant): Int32 =
      val p1 = smooth(from = 0.355f, to = 0.4f, duration = 10.seconds)
      val p2 = smooth(from = 0.4f, to = 0.355f, duration = 10.seconds, at = 10.seconds)
      val const1 = (p1, p1)
      val const2 = (p2, p2)
      GSeq.gen(uv, next = v => {
        when(!(p1 === 0.4f)){
          val abs_v = (abs(v.x), abs(v.y))
          ((abs_v.x * abs_v.x) - (abs_v.y * abs_v.y), 2.0f * abs_v.x * abs_v.y) + const1
        }otherwise{
          val abs_v = (abs(v.x), abs(v.y))
          ((abs_v.x * abs_v.x) - (abs_v.y * abs_v.y), 2.0f * abs_v.x * abs_v.y) + const2
        }
        
      }).limit(1000).map(length).takeWhile(_ < 2.0f).count

    def connectingFormsColor(uv: Vec2[Float32])(using AnimationInstant): Vec4[Float32] =
      val rotatedUv = rotate(uv, Math.PI.toFloat / 3.0f)
      val recursionCount = connectingForms(rotatedUv)
      val f = min(1f, recursionCount.asFloat / 100f)
      val color = interpolate(InterpolationThemes.Red, f)
      (
        color.r,
        color.g,
        color.b,
        1.0f
      )

    val animatedConnectingForms = AnimatedFunction.fromCoord(connectingFormsColor, 20.seconds)

    val renderer = AnimatedFunctionRenderer(Parameters(1024, 1024, 5))
    renderer.renderFramesToDir(animatedConnectingForms, Paths.get("connectingForms"))

