package io.computenode.cyfra.e2e

import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.core.archive.*
import mem.*
import io.computenode.cyfra.dsl.{*, given}

class WhenE2eLegacyTest extends munit.FunSuite:
  given gc: GContext = GContext()

  test("when elseWhen otherwise (legacy)"):
    val oneHundred = 100.0f
    val twoHundred = 200.0f
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      when(f <= oneHundred)(0.0f)
        .elseWhen(f <= twoHundred)(1.0f)
        .otherwise(2.0f)

    val inArr: Array[Float] = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr.map: f =>
      if f <= oneHundred then 0.0f else if f <= twoHundred then 1.0f else 2.0f
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")
