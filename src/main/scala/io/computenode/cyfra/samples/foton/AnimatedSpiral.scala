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

object AnimatedSpiral:
  @main
  def Spiral() =
    def spiral(uv: Vec2[Float32])(using a: AnimationInstant): Vec4[Float32] =
      val centered = uv 
      val angle = atan2(centered.y, centered.x)
      val radius = length(centered)
      val f = abs(sin(10f * radius + 5f * angle + a.time))
      val color = interpolate(InterpolationThemes.Black, f)
      val rotatedUv = rotate(uv, Math.PI.toFloat / 3.0f)
      (
        color.r,
        color.g,
        color.b,
        1.0f
      )

    val animatedSpiral = AnimatedFunction.fromCoord(spiral, 2.seconds)

    val renderer = AnimatedFunctionRenderer(Parameters(1024, 1024, 80))
    renderer.renderFramesToDir(animatedSpiral, Paths.get("animatedSpiral"))

