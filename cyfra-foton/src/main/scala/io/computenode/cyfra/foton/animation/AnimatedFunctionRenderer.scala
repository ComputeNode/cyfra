package io.computenode.cyfra.foton.animation


import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra
import io.computenode.cyfra.dsl.{GStruct, UniformContext, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.animation.AnimatedFunctionRenderer.{AnimationIteration, RenderFn}
import io.computenode.cyfra.foton.animation.AnimationFunctions.AnimationInstant
import io.computenode.cyfra.foton.animation.AnimationRenderer
import io.computenode.cyfra.foton.rt.ImageRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.animation.AnimationRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.RtRenderer
import io.computenode.cyfra.runtime.{GArray2DFunction, GContext, MVPContext}
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.runtime.mem.GMem.fRGBA
import io.computenode.cyfra.runtime.mem.Vec4FloatMem

import java.nio.file.{Path, Paths}
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt


class AnimatedFunctionRenderer(params: AnimatedFunctionRenderer.Parameters) extends AnimationRenderer[AnimatedFunction, AnimatedFunctionRenderer.RenderFn](params):

  given GContext = new MVPContext()

  given ExecutionContext = Implicits.global
  
  protected override def renderFrame(scene: AnimatedFunction, time: Float32, fn: RenderFn): Array[fRGBA] =
    val mem = Array.fill(params.width * params.height)((0.5f, 0.5f, 0.5f, 0.5f))
    UniformContext.withUniform(AnimationIteration(time)):
      val fmem = Vec4FloatMem(mem)
      Await.result(fmem.map(fn), 1.minute)

  protected override def renderFunction(scene: AnimatedFunction): RenderFn = 
    GArray2DFunction(params.width, params.height, {
      case (AnimationIteration(time), (xi: Int32, yi: Int32), lastFrame) =>
        val lastColor = lastFrame.at(xi, yi)
        val x = (xi - (params.width / 2)).asFloat / params.width.toFloat
        val y = (yi - (params.height / 2)).asFloat / params.height.toFloat
        val uv = (x, y)
        scene.fn(AnimatedFunction.FunctionArguments(lastFrame, lastColor, uv))(using AnimationInstant(time))
    })

object AnimatedFunctionRenderer:
  type RenderFn = GArray2DFunction[AnimationIteration, Vec4[Float32], Vec4[Float32]]
  case class AnimationIteration(time: Float32) extends GStruct[AnimationIteration]

  case class Parameters(
    width: Int,
    height: Int,
    framesPerSecond: Int
  ) extends AnimationRenderer.Parameters