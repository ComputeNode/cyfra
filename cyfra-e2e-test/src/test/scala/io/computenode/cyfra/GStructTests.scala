package io.computenode.cyfra.e2e

import io.computenode.cyfra.runtime.*, mem.*
import io.computenode.cyfra.dsl.{*, given}
import GStruct.Empty.given

class GStructE2eTest extends munit.FunSuite:
  case class Custom(f: Float32, v: Vec4[Float32]) extends GStruct[Custom]
  val custom1 = Custom(2f, (1f, 2f, 3f, 4f))
  val custom2 = Custom(-0.5f, (-0.5f, -1.5f, -2.5f, -3.5f))

  case class Nested(c1: Custom, c2: Custom) extends GStruct[Nested]
  val nested = Nested(custom1, custom2)

  given gc: GContext = GContext()

  test("custom GStruct"):
    UniformContext.withUniform(custom1):
      val gf: GFunction[Custom, Float32, Float32] = GFunction:
        case (Custom(f, v), index, gArray) => v.*(f).dot(v) + gArray.at(index) * f

      val inArr = (0 to 255).map(_.toFloat).toArray
      val gmem = FloatMem(inArr)
      val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

      val expected = inArr.map(f => 2f * f + 60f)
      result
        .zip(expected)
        .foreach: (res, exp) =>
          assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")

  test("GStruct of GStructs".ignore):
    UniformContext.withUniform(nested):
      val gf: GFunction[Nested, Float32, Float32] = GFunction:
        case (Nested(Custom(f1, v1), Custom(f2, v2)), index, gArray) =>
          v1.*(f2).dot(v2) + gArray.at(index) * f1

      val inArr = (0 to 255).map(_.toFloat).toArray
      val gmem = FloatMem(inArr)
      val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

      val expected = inArr.map(f => 2f * f + 12.5f)
      result
        .zip(expected)
        .foreach: (res, exp) =>
          assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")

  test("GSeq of GStructs"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: fl =>
      GSeq
        .gen(custom1, c => Custom(c.f * 2f, c.v.*(2f)))
        .limit(3)
        .fold[Float32](0f, (f, c) => f + c.f * (c.v.w + c.v.x + c.v.y + c.v.z))
      + fl

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr.map(f => f + 420f)
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp but got $res")
