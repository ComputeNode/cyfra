package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class ConversionTest extends munit.FunSuite:
  test("same-type floating conversions preserve their expression"):
    val half = Float16(1.5f)
    val single = Float32(2.5f)
    val vector = Vec4(single)
    val halfVector = vector.asFloat16
    assertEquals(Value[Float16].peel(half.asFloat16), Value[Float16].peel(half))
    assertEquals(Value[Float32].peel(single.asFloat32), Value[Float32].peel(single))
    assertEquals(Value[Vec4[Float32]].peel(vector.asFloat32), Value[Vec4[Float32]].peel(vector))
    assertEquals(Value[Vec4[Float16]].peel(halfVector.asFloat16), Value[Vec4[Float16]].peel(halfVector))

  test("different-width floating conversions still create a conversion"):
    val converted = Value[Float16].peel(Float32(1.5f).asFloat16).result
    assert(converted.isInstanceOf[Expression.Operation[?]])
    assertEquals(converted.asInstanceOf[Expression.Operation[Float16]].op, Operator.Convert)
