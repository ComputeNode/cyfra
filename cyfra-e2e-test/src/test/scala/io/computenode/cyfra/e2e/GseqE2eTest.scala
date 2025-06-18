package io.computenode.cyfra.e2e

import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.runtime.*
import mem.*
import io.computenode.cyfra.dsl.{*, given}

class GseqE2eTest extends munit.FunSuite:
  given gc: GContext = GContext()

  test("GSeq gen limit map fold"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      GSeq
        .gen(f, _ + 1.0f)
        .limit(10)
        .map(_ + 2.0f)
        .fold[Float32](0f, _ + _)

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray

    val expected = inArr.map(f => 10 * f + 65.0f)
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")

  test("GSeq of takeWhile filter count"):
    val gf: GFunction[GStruct.Empty, Int32, Int32] = GFunction: n =>
      GSeq
        .of(List.iterate(n, 10)(_ + 1))
        .takeWhile(_ <= 200)
        .filter(_.mod(2) === 0)
        .count

    val inArr = (0 to 255).toArray
    val gmem = IntMem(inArr)
    val result = gmem.map(gf).asInstanceOf[IntMem].toArray

    val expected = inArr.map: n =>
      List
        .iterate(n, 10)(_ + 1)
        .takeWhile(_ <= 200)
        .count(_ % 2 == 0)

    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp but got $res")
