package io.computenode.cyfra.e2e

import io.computenode.cyfra.runtime.*, mem.*
import io.computenode.cyfra.dsl.{*, given}
import GStruct.Empty.given

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
        assert(Math.abs(res - exp) < 0.01f, s"Expected $exp but got $res")

  test("smoothstep clamp mix reflect refract normalize"): // STUB
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      0f

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr.map: f =>
      0f

    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.01f, s"Expected $exp but got $res")
