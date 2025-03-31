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

//This pattern was adapted from https://www.shadertoy.com/view/XsXXDn by Silexars

object AnimatedWaveCircle:
  @main
  def animatedWaveCircle() =
    def waveColor(uv: Vec2[Float32])(using t: AnimationInstant): Vec4[Float32] =
      val aspect = 840f / 473f
      val p = (uv.x * aspect, uv.y)
      var c = Array[Float32](0f, 0f, 0f)
      val l = length(p)
      var z = t.time
      var uvCompute = p
      
      for i <- 0 until 3 do
        z += 0.07f * (i + 1).toFloat
        val multiplyFactor = (sin(z) + 1f) * abs(sin(l * 9f - 2f * z))
        uvCompute =  (uvCompute.y +(p.x * (1f/l)) * multiplyFactor, uvCompute.x + (p.y * (1f/l)) * multiplyFactor)
        val uvMod = (uvCompute.x.mod(1f), uvCompute.y.mod(1f))
        c(i) = 0.01f / length((uvMod._1 - 0.5f, uvMod._2 - 0.5f))
      
      (c(0) / l, c(1) / l, c(2) / l, t.time)
    
    val animatedStar = AnimatedFunction.fromCoord(waveColor, 8.milliseconds)
    val renderer = AnimatedFunctionRenderer(Parameters(840, 473, 30000))
    renderer.renderFramesToDir(animatedStar, Paths.get("animatedWaveCircle"))