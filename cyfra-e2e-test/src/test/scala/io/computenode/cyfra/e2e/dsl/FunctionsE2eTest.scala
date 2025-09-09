package io.computenode.cyfra.e2e.dsl

import io.computenode.cyfra.core.archive.*
import io.computenode.cyfra.core.archive.mem.*
import io.computenode.cyfra.core.archive.mem.GMem.fRGBA
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}

class FunctionsE2eTest extends munit.FunSuite:
  given gc: GContext = GContext()

  test("Functions"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      val res1 = pow(sqrt(exp(sin(cos(tan(f))))), 2f)
      val res2 = logn(asin(acos(atan(f) / 2f) / 4f) + 2f)
      abs(min(res1, res2) - max(res1, res2))

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr.map: f =>
      val res1 = math.pow(math.sqrt(math.exp(math.sin(math.cos(math.tan(f))))), 2)
      val res2 = math.log(math.asin(math.acos(math.atan(f) / 2f) / 4f) + 2f)
      math.abs(math.min(res1, res2) - math.max(res1, res2))

    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.05f, s"Expected $exp but got $res")

  test("smoothstep clamp mix reflect refract normalize"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      val f1 = smoothstep(f - 1.2f, f + 3.4f, f)
      val f2 = mix(f, f + 1f, f1)
      val v1: Vec4[Float32] = (f1, f2, f1 + 3.4f, f2 - 1.2f)
      val v2: Vec4[Float32] = (f1 * 3.4f, f1, f2, f2 / 1.2f)
      val v3 = mix(v1, v2, f2)
      val v4 = reflect(v3, v2)
      val eta: Float32 = 0.01f
      val v5 = refract(normalize(v3), normalize(v4), eta)
      v5.dot(v1)

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    extension (f: fRGBA)
      def neg: fRGBA = (-f._1, -f._2, -f._3, -f._4)
      def add(that: fRGBA) = (f._1 + that._1, f._2 + that._2, f._3 + that._3, f._4 + that._4)
      def sub(that: fRGBA) = f.add(that.neg)
      def scl(s: Float) = (f._1 * s, f._2 * s, f._3 * s, f._4 * s)
      def div(s: Float): fRGBA = (f._1 / s, f._2 / s, f._3 / s, f._4 / s)
      def pro(that: fRGBA) = f._1 * that._1 + f._2 * that._2 + f._3 * that._3 + f._4 * that._4
      def norm: Float = math.sqrt(f.pro(f)).toFloat
      def normalized: fRGBA = f.div(f.norm)
      def reflected(n: fRGBA): fRGBA = f.sub(n.scl(2 * n.pro(f)))
      def refracted(n: fRGBA, eta: Float): fRGBA = // assumes f, n are normalized
        val dotted = n.pro(f)
        val k = 1.0f - eta * eta * (1.0f - dotted * dotted)
        if k < 0.0 then (0f, 0f, 0f, 0f)
        else f.scl(eta).sub(n.scl(eta * dotted + math.sqrt(k).toFloat))

    def floatMix(a: Float, b: Float, t: Float): Float = a * (1 - t) + b * t
    def vecMix(a: fRGBA, b: fRGBA, t: Float): fRGBA = a.scl(1 - t).add(b.scl(t))
    def scalaClamp(f: Float, from: Float, to: Float) = math.min(math.max(f, from), to)
    def scalaSmooth(e0: Float, e1: Float, x: Float): Float =
      val t = scalaClamp((x - e0) / (e1 - e0), 0f, 1f)
      t * t * (3 - 2 * t)

    val expected = inArr.map: f =>
      val f1 = scalaSmooth(f - 1.2f, f + 3.4f, f)
      val f2 = floatMix(f, f + 1f, f1)
      val v1 = (f1, f2, f1 + 3.4f, f2 - 1.2f)
      val v2 = (f1 * 3.4f, f1, f2, f2 / 1.2f)
      val v3 = vecMix(v1, v2, f2)
      val v4 = v3.reflected(v2)
      val eta = 0.01f
      val v5 = v3.normalized.refracted(v4.normalized, eta)
      v5.pro(v1)

    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.0001f, s"Expected $exp but got $res")
