package io.computenode.cyfra.foton.animation

import io.computenode.cyfra
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.collections.GArray2D
import io.computenode.cyfra.foton.animation.AnimatedFunction.FunctionArguments
import io.computenode.cyfra.foton.animation.AnimationFunctions.AnimationInstant
import io.computenode.cyfra.utility.Units.Milliseconds

case class AnimatedFunction(fn: FunctionArguments => AnimationInstant ?=> Vec4[Float32], duration: Milliseconds) extends AnimationRenderer.Scene

object AnimatedFunction:
  case class FunctionArguments(data: GArray2D[Vec4[Float32]], color: Vec4[Float32], uv: Vec2[Float32])

  def fromCoord(fn: Vec2[Float32] => AnimationInstant ?=> Vec4[Float32], duration: Milliseconds): AnimatedFunction =
    AnimatedFunction(args => fn(args.uv), duration)

  def fromColor(fn: Vec4[Float32] => AnimationInstant ?=> Vec4[Float32], duration: Milliseconds): AnimatedFunction =
    AnimatedFunction(args => fn(args.color), duration)

  def fromData(fn: (GArray2D[Vec4[Float32]], Vec2[Float32]) => AnimationInstant ?=> Vec4[Float32], duration: Milliseconds): AnimatedFunction =
    AnimatedFunction(args => fn(args.data, args.uv), duration)
