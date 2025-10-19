package io.computenode.cyfra.e2e.dsl

import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.core.archive.GFunction
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.core.GCodec.{*, given}

class WhenE2eTest extends munit.FunSuite:
  given CyfraRuntime = VkCyfraRuntime()

  test("when elseWhen otherwise"):
    val oneHundred = 100.0f
    val twoHundred = 200.0f
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      when(f <= oneHundred)(0.0f)
        .elseWhen(f <= twoHundred)(1.0f)
        .otherwise(2.0f)

    val inArr: Array[Float] = (0 to 255).map(_.toFloat).toArray
    val result: Array[Float] = gf.run(inArr)

    val expected = inArr.map: f =>
      if f <= oneHundred then 0.0f else if f <= twoHundred then 1.0f else 2.0f
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")
