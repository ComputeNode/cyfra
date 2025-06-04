package io.computenode.cyfra.foton.rt.animation

import io.computenode.cyfra
import io.computenode.cyfra.dsl.{GStruct, UniformContext}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.animation.AnimationRenderer
import io.computenode.cyfra.foton.rt.ImageRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.animation.AnimationRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.RtRenderer
import io.computenode.cyfra.runtime.GFunction
import io.computenode.cyfra.runtime.mem.GMem.fRGBA
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.runtime.mem.Vec4FloatMem
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.GStruct.{*, given}
import io.computenode.cyfra.dsl.given

import java.nio.file.{Path, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class AnimationRtRenderer(params: AnimationRtRenderer.Parameters) extends RtRenderer(params) with AnimationRenderer[AnimatedScene, AnimationRtRenderer.RenderFn](params):

  protected def renderFrame(
    scene: AnimatedScene,
    time: Float32,
    fn: GFunction[RaytracingIteration, Vec4[Float32], Vec4[Float32]]
  ): Array[fRGBA] =
    val initialMem = Array.fill(params.width * params.height)((0.5f, 0.5f, 0.5f, 0.5f))
    List.iterate((initialMem, 0), params.iterations + 1) { case (mem, render) =>
      val uniformStruct = RaytracingIteration(render, time) 
      UniformContext.withUniform(uniformStruct): 
        val fmem = Vec4FloatMem(mem)
        val result = fmem.map(uniformStruct, fn).asInstanceOf[Vec4FloatMem].toArray
        (result, render + 1)
    }.map(_._1).last


  protected def renderFunction(scene: AnimatedScene): GFunction[RaytracingIteration, Vec4[Float32], Vec4[Float32]] =
    GFunction.from2D(params.width, {
      case (RaytracingIteration(frame, time), (xi: Int32, yi: Int32), lastFrame) =>
        renderFrame(xi, yi, frame, lastFrame, scene.at(time))
    })

object AnimationRtRenderer:
  
  type RenderFn = GFunction[RaytracingIteration, Vec4[Float32], Vec4[Float32]]
  case class RaytracingIteration(frame: Int32, time: Float32) extends GStruct[RaytracingIteration]

  case class Parameters(
    width: Int,
    height: Int,
    fovDeg: Float = 60.0f,
    superFar: Float = 1000.0f,
    maxBounces: Int = 8,
    pixelIterations: Int = 1000,
    iterations: Int = 5,
    bgColor: (Float, Float, Float) = (0.2f, 0.2f, 0.2f),
    framesPerSecond: Int = 20
  ) extends RtRenderer.Parameters with AnimationRenderer.Parameters
