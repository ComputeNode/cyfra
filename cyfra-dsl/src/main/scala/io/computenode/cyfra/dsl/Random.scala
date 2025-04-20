package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.{GStruct, Value, *, given}


case class Random(seed: UInt32) extends GStruct[Random]:
  def next[R <: Value : Random.Generator]: (Random, R) =
    val generator = summon[Random.Generator[R]]
    val (nextValue, nextSeed) = generator.gen(seed)
    (Random(nextSeed), nextValue)

object Random:
  trait Generator[T <: Value]:
    def gen(seed: UInt32): (T, UInt32)

    def wangHash(seed: UInt32): UInt32 = {
      val s1 = (seed ^ 61) ^ (seed >> 16)
      val s2 = s1 * 9
      val s3 = s2 ^ (s2 >> 4)
      val s4 = s3 * 0x27d4eb2d
      s4 ^ (s4 >> 15)
    }
    
  given Generator[Float32] with
    def gen(seed: UInt32): (Float32, UInt32) =
      val nextSeed = wangHash(seed)
      val f = nextSeed.asFloat / 4294967296.0f
      (f, nextSeed)
      
  given Generator[Vec3[Float32]] with
    def gen(seed: UInt32): (Vec3[Float32], UInt32) =
      val floatGenerator = summon[Generator[Float32]]
      val (z, seed1) = floatGenerator.gen(seed)
      val z2 = z * 2.0f - 1.0f
      val (a, seed2) = floatGenerator.gen(seed1)
      val a2 = a * 2.0f * math.Pi.toFloat
      val r = sqrt(1.0f - z2 * z2)
      val x = r * cos(a2)
      val y = r * sin(a2)
      ((x, y, z2), seed2)