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

object AnimatedJuliaSin:
  @main
  def juliaSin() =

    def juliaSin(uv: Vec2[Float32])(using AnimationInstant): Int32 =
      val p = smooth(from = 0.355f, to = 0.4f, duration = 3.seconds)
      val const = (p, p)
      GSeq.gen(uv, next = v => {
        // Define the Julia set transformation here
        ((v.x * v.x ) - (v.y * v.y), 2.0f * v.x * v.y) + const
      }).limit(1000).map(length).takeWhile(_ < 2.0f).count

    def juliaSinColor(uv: Vec2[Float32])(using AnimationInstant): Vec4[Float32] =
      val rotatedUv = rotate(uv, Math.PI.toFloat / 3.0f)
      val recursionCount = juliaSin(rotatedUv)
      val f = min(1f, recursionCount.asFloat / 100f)
      
      val brightness = vec3(0.5f, 0.5f, 0.5f) // Adjust as needed
      val contrast = vec3(0.5f, 0.5f, 0.5f)   // Increase for more vivid colors
      val freq = vec3(3.0f, 2.0f, 1.0f)       // Controls the color cycling
      val offsets = vec3(0.0f, 0.5f, 1.0f)    // Phase offsets for R, G, B

      val color1 = igPallette(brightness, contrast, freq, offsets, f)
      (
        color1.r,
        color1.g,
        color1.b,
        1.0f
      )

    val animatedJuliaSin = AnimatedFunction.fromCoord(juliaSinColor, 3.seconds)

    val renderer = AnimatedFunctionRenderer(Parameters(1024, 1024, 30)) // Reduce resolution if our GPU is not capabale    
    renderer.renderFramesToDir(animatedJuliaSin, Paths.get("juliaSin"))

// ffmpeg -framerate 30 -i frame%02d.png -vf "scale=540:540" -c:v libx264 -pix_fmt yuv420p output_video6.mp4
