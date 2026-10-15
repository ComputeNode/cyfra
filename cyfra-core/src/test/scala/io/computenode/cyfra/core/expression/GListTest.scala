package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class GListTest extends munit.FunSuite:
  test("fixed-list type identity includes length and stride includes nested elements"):
    assert(Value[GList[Float32, 4]].tag =:= Value[GList[Float32, 4]].tag)
    assert(!(Value[GList[Float32, 4]].tag =:= Value[GList[Float32, 8]].tag))
    assertEquals(typeStride(Value[GList[Float32, 4]]), 16)
    assertEquals(typeStride(Value[GList[Vec3[Float32], 4]]), 48)
    assertEquals(typeStride(Value[GList[Mat2x3[Float32], 4]]), 96)
    assertEquals(typeStride(Value[Mat3x2[Float16]]), 12)
    assertEquals(typeStride(Value[GList[GList[Float16, 3], 2]]), 12)
    assertEquals(typeStride(Value[GList[(Float32, Int32), 4]]), 32)

  test("fixed-list lengths must be positive"):
    intercept[IllegalArgumentException](Value[GList[Float32, 0]])
    intercept[IllegalArgumentException](Value[GList[Float32, -1]])

  test("fixed-list values preserve their expression block and expose their length"):
    val value = Value[GList[Float32, 4]]
    val root = io.computenode.cyfra.core.memory.LocalVariable[GList[Float32, 4]]()
    val block = ExpressionBlock(Expression.Read[GList[Float32, 4]](root, Nil))
    val list = value.extractUnchecked(block)
    assertEquals(value.peel(list), block)
    assertEquals(list.length, 4)
    assertEquals(value.arrayLength, Some(4))
