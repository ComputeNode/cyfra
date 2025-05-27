package io.computenode.cyfra.e2e

import io.computenode.cyfra.runtime.*, mem.*
import io.computenode.cyfra.dsl.{*, given}
import GStruct.Empty.given

class GStructE2eTest extends munit.FunSuite:
  given gc: GContext = GContext()

  test("custom GStruct"):
    case class Custom(f: Float32, v: Vec4[Float32]) extends GStruct[Custom]

    UniformContext.withUniform(Custom(2f, (1f, 2f, 3f, 4f))):
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

  test("GStruct of GStructs"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      ???

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp but got $res")

  test("GSeq of GStructs"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      ???

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp but got $res")
