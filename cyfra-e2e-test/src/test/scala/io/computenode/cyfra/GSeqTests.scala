package io.computenode.cyfra.e2e

import io.computenode.cyfra.runtime.*, mem.*
import io.computenode.cyfra.dsl.{*, given}
import GStruct.Empty.given

class GseqE2eTest extends munit.FunSuite:
  given gc: GContext = GContext()

  test("GSeq"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      GSeq
        .gen(f, _ + 1.0f)
        .limit(10)
        .fold[Float32](0f, _ + _)

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr.map(f => 10 * f + 45.0f) // this is probably wrong
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")
