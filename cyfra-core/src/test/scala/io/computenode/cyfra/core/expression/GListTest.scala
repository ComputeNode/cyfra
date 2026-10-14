package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class GListTest extends munit.FunSuite:
  test("fixed-list type identity includes length and stride includes nested elements"):
    assert(Value[GList[Float32, 4]].tag =:= Value[GList[Float32, 4]].tag)
    assert(!(Value[GList[Float32, 4]].tag =:= Value[GList[Float32, 8]].tag))
    assertEquals(typeStride(Value[GList[Float32, 4]]), 16)
    assertEquals(typeStride(Value[GList[GList[Float16, 3], 2]]), 12)
    assertEquals(typeStride(Value[GList[(Float32, Int32), 4]]), 32)

  test("fixed-list lengths must be positive"):
    intercept[IllegalArgumentException](Value[GList[Float32, 0]])
    intercept[IllegalArgumentException](Value[GList[Float32, -1]])
