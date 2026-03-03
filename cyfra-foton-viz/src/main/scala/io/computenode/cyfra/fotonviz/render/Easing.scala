package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}

/** Animation easing and timing functions. */
object Easing:

  /** Heartbeat: sharp attack with exponential decay, repeating at given period. */
  def heartbeat(
    time: Float32,
    period: Float32,
    attack: Float32 = 1.1f,
    decay: Float32 = 2.5f,
  ): Float32 =
    val cycleStart = (time / period).asInt.asFloat * period
    val phase = time - cycleStart
    val x = phase * attack
    clamp(x * exp(-x * decay) * 4.0f, 0.0f, 1.0f)

  /** Double heartbeat (lub-dub): two peaks per cycle. */
  def lubDub(
    time: Float32,
    period: Float32,
    attack: Float32 = 8.0f,
    decay: Float32 = 2.5f,
    dubDelay: Float32 = 0.15f,
    dubScale: Float32 = 0.5f,
  ): Float32 =
    val cycleStart = (time / period).asInt.asFloat * period
    val phase = time - cycleStart
    // First peak (lub)
    val lub = phase * attack
    val lubPeak = lub * exp(-lub * decay) * 4.0f
    // Second peak (dub)
    val dubPhase = max(phase - dubDelay, 0.0f) * attack * 1.25f
    val dubPeak = dubPhase * exp(-dubPhase * decay * 1.2f) * 4.0f * dubScale
    clamp(lubPeak + dubPeak, 0.0f, 1.0f)

  /** Smooth oscillation between 0 and 1. */
  def smoothPulse(time: Float32, period: Float32): Float32 =
    sin(time * (3.14159f * 2.0f) / period) * 0.5f + 0.5f

  /** Triangle wave between 0 and 1. */
  def triangle(time: Float32, period: Float32): Float32 =
    val t = (time / period) - (time / period).asInt.asFloat
    abs(t * 2.0f - 1.0f)

  /** Ease in (quadratic). */
  def easeIn(t: Float32): Float32 = t * t

  /** Ease out (quadratic). */
  def easeOut(t: Float32): Float32 = 1.0f - (1.0f - t) * (1.0f - t)

  /** Ease in-out (smoothstep). */
  def smoothstep(t: Float32): Float32 = t * t * (3.0f - 2.0f * t)

  /** Bounce effect. */
  def bounce(t: Float32): Float32 =
    val t2 = abs(sin(t * 3.14159f * 2.0f))
    t2 * exp(-t * 3.0f)
