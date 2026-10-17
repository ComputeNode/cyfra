package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class ConversionTest extends munit.FunSuite:
  test("different-width floating conversions create a conversion"):
    val converted = Value[Float16].peel(Float32(1.5f).asFloat16).result
    assert(converted.isInstanceOf[Expression.Operation[?]])
    assertEquals(converted.asInstanceOf[Expression.Operation[Float16]].op, Operator.Convert)
