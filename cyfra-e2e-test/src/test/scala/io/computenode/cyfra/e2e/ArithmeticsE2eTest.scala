package io.computenode.cyfra.e2e

import io.computenode.cyfra.core.archive.*
import mem.*
import GMem.fRGBA
import io.computenode.cyfra.dsl.algebra.VectorAlgebra
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}

class ArithmeticsE2eLegacyTest extends munit.FunSuite:
  given gc: GContext = GContext()

  test("Float32 arithmetics (legacy)"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: fl =>
      (fl + 1.2f) * (fl - 3.4f) / 5.6f

    // We need to use multiples of 256 for Vulkan buffer alignment.
    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr.map(f => (f + 1.2f) * (f - 3.4f) / 5.6f)
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")

  test("Int32 arithmetics (legacy)"):
    val gf: GFunction[GStruct.Empty, Int32, Int32] = GFunction: n =>
      ((n + 2) * (n - 3) / 5).mod(7)

    val inArr = (0 to 255).toArray
    val gmem = IntMem(inArr)
    val result = gmem.map(gf).asInstanceOf[IntMem].toArray

    // With negative values and mod, Scala and Vulkan behave differently
    val expected = inArr.map: n =>
      val res = ((n + 2) * (n - 3) / 5) % 7
      res + (if res < 0 then 7 else 0)

    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp but got $res")

  test("Vec4Float32 arithmetics (legacy)"):
    val f1 = (1.2f, 2.3f, 3.4f, 4.5f)
    val f2 = (5.6f, 6.7f, 7.8f, 8.9f)
    val f3 = (-5.3f, 6.2f, -4.7f, 9.1f)
    val sc = -2.1f

    val v1 = VectorAlgebra.vec4.tupled(f1)
    val v2 = VectorAlgebra.vec4.tupled(f2)
    val v3 = VectorAlgebra.vec4.tupled(f3)

    val gf: GFunction[GStruct.Empty, Vec4[Float32], Float32] = GFunction: v4 =>
      (-v4).*(sc).+(v1).-(v2).dot(v3)

    val inArr: Array[fRGBA] = (0 to 1023)
      .map(_.toFloat)
      .grouped(4)
      .map:
        case Seq(a, b, c, d) => (a, b, c, d)
      .toArray

    val gmem = Vec4FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    extension (f: fRGBA)
      def neg = (-f._1, -f._2, -f._3, -f._4)
      def scl(s: Float) = (f._1 * s, f._2 * s, f._3 * s, f._4 * s)
      def add(g: fRGBA) = (f._1 + g._1, f._2 + g._2, f._3 + g._3, f._4 + g._4)
      def sub(g: fRGBA) = (f._1 - g._1, f._2 - g._2, f._3 - g._3, f._4 - g._4)
      def pro(g: fRGBA) = f._1 * g._1 + f._2 * g._2 + f._3 * g._3 + f._4 * g._4

    val expected: Array[Float] = inArr.map(f => f.neg.scl(sc).add(f1).sub(f2).pro(f3))

    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.01f, s"Expected $exp but got $res")
