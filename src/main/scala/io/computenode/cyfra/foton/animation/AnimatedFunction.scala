package io.computenode.cyfra.foton.animation

import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.GArray2D
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.foton.animation.AnimatedFunction.FunctionArguments
import io.computenode.cyfra.foton.animation.AnimationFunctions.AnimationInstant
import io.computenode.cyfra.foton.animation.AnimationRenderer
import io.computenode.cyfra.foton.rt.ImageRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.animation.AnimationRtRenderer.RaytracingIteration
import io.computenode.cyfra.foton.rt.RtRenderer
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.{*, given}

import java.nio.file.{Path, Paths}
import scala.annotation.targetName
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

case class AnimatedFunction(
  fn: FunctionArguments => AnimationInstant ?=> Vec4[Float32],
  duration: Milliseconds
) extends AnimationRenderer.Scene


object AnimatedFunction:
  case class FunctionArguments(
    data: GArray2D[Vec4[Float32]],
    color: Vec4[Float32],
    uv: Vec2[Float32]
  )

  def fromCoord(fn: Vec2[Float32] => AnimationInstant ?=> Vec4[Float32], duration: Milliseconds): AnimatedFunction =
    AnimatedFunction(
      args => fn(args.uv),
      duration
    )
    
  def fromColor(fn: Vec4[Float32] => AnimationInstant ?=> Vec4[Float32], duration: Milliseconds): AnimatedFunction =
    AnimatedFunction(
      args => fn(args.color),
      duration
    )
    
  def fromData(fn: (GArray2D[Vec4[Float32]], Vec2[Float32]) => AnimationInstant ?=> Vec4[Float32], duration: Milliseconds): AnimatedFunction =
    AnimatedFunction(
      args => fn(args.data, args.uv),
      duration
    )