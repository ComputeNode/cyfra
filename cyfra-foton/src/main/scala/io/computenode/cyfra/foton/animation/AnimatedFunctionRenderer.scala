package io.computenode.cyfra.foton.animation

import io.computenode.cyfra
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.animation.AnimatedFunctionRenderer.{AnimationIteration, RenderFn}
import io.computenode.cyfra.foton.animation.AnimationFunctions.AnimationInstant
import io.computenode.cyfra.core.aalegacy.mem.GMem.fRGBA
import io.computenode.cyfra.core.aalegacy.mem.Vec4FloatMem
import io.computenode.cyfra.core.aalegacy.{GContext, GFunction, UniformContext}

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

class AnimatedFunctionRenderer(params: AnimatedFunctionRenderer.Parameters)
    extends AnimationRenderer[AnimatedFunction, AnimatedFunctionRenderer.RenderFn](params):

  given GContext = new GContext()

  given ExecutionContext = Implicits.global

  override protected def renderFrame(scene: AnimatedFunction, time: Float32, fn: RenderFn): Array[fRGBA] =
    val mem = Array.fill(params.width * params.height)((0.5f, 0.5f, 0.5f, 0.5f))
    UniformContext.withUniform(AnimationIteration(time)):
      val fmem = Vec4FloatMem(mem)
      fmem.map(fn).asInstanceOf[Vec4FloatMem].toArray

  override protected def renderFunction(scene: AnimatedFunction): RenderFn =
    GFunction.from2D(params.width):
      case (AnimationIteration(time), (xi: Int32, yi: Int32), lastFrame) =>
        val lastColor = lastFrame.at(xi, yi)
        val x = (xi - (params.width / 2)).asFloat / params.width.toFloat
        val y = (yi - (params.height / 2)).asFloat / params.height.toFloat
        val uv = (x, y)
        scene.fn(AnimatedFunction.FunctionArguments(lastFrame, lastColor, uv))(using AnimationInstant(time))

object AnimatedFunctionRenderer:
  type RenderFn = GFunction[AnimationIteration, Vec4[Float32], Vec4[Float32]]
  case class AnimationIteration(time: Float32) extends GStruct[AnimationIteration]

  case class Parameters(width: Int, height: Int, framesPerSecond: Int) extends AnimationRenderer.Parameters
