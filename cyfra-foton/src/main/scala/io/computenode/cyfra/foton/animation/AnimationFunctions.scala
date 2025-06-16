package io.computenode.cyfra.foton.animation

import io.computenode.cyfra.given
import io.computenode.cyfra
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.foton.rt.animation.AnimationRtRenderer.RaytracingIteration
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Value.Float32
import io.computenode.cyfra.foton.rt.ImageRtRenderer.RaytracingIteration
import io.computenode.cyfra.utility.Units.Milliseconds
import io.computenode.cyfra.utility.Utility.timed
import io.computenode.cyfra.foton.rt.RtRenderer

object AnimationFunctions:

  case class AnimationInstant(time: Float32)

  def smooth(from: Float32, to: Float32, duration: Milliseconds, at: Milliseconds = Milliseconds(0)): AnimationInstant ?=> Float32 =
    inst ?=>
      val t = inst.time
      when(t > at && t <= (at + duration)):
        val p = (t - at) / duration
        val dist = to - from
        from + (dist * p)
      .elseWhen(t <= at):
        from
      .otherwise:
        to

//  def freefall(from: Float32, to: Float32, g: Float32): Float32 => Vec3[Float32] =
//    t =>
//      val distance = to - from
//      val t0 = 2f * sqrt(distance / g)
//      val n = log(t / t0 + 1f, 2f)
//      val factor = 1f - pow(2f, -n)
//      val p = pow(2f, -n) * (t / t0 + 1f) - 1f
//      val v = g * t
//      val s = from + v * t / 2f
//      vec3(s, v, factor)
//
//  def bounceFreefall(from: Float32, to: Float32, g: Float32, bounciness: Float32): Float32 => Vec3[Float32] =
//    t =>
//      val distance = to - from
//      val t0 = 2f * sqrt(distance / g)
//      val factor = 1f - sqrt(bounciness)
//      val n = log((t * factor) / t0 + 1f, sqrt(bounciness)).asInt
//      ???
