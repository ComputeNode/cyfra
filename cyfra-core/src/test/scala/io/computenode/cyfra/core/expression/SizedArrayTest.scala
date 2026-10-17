package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class SizedArrayTest extends munit.FunSuite:
  test("sized-array type identity includes length and stride includes nested elements"):
    assert(Value[SizedArray[Float32, 4]].tag =:= Value[SizedArray[Float32, 4]].tag)
    assert(!(Value[SizedArray[Float32, 4]].tag =:= Value[SizedArray[Float32, 8]].tag))
    assertEquals(typeStride(Value[SizedArray[Float32, 4]]), 16)
    assertEquals(typeStride(Value[SizedArray[Vec3[Float32], 4]]), 48)
    assertEquals(typeStride(Value[SizedArray[Mat2x3[Float32], 4]]), 96)
    assertEquals(typeStride(Value[Mat3x2[Float16]]), 12)
    assertEquals(typeStride(Value[SizedArray[SizedArray[Float16, 3], 2]]), 12)
    assertEquals(typeStride(Value[SizedArray[(Float32, Int32), 4]]), 32)

  test("sized-array lengths must be positive"):
    intercept[IllegalArgumentException](Value[SizedArray[Float32, 0]])
    intercept[IllegalArgumentException](Value[SizedArray[Float32, -1]])

  test("sized-array values preserve their expression block and expose their length"):
    val value = Value[SizedArray[Float32, 4]]
    val root = io.computenode.cyfra.core.memory.LocalVariable[SizedArray[Float32, 4]]()
    val block = ExpressionBlock(Expression.Read[SizedArray[Float32, 4]](root, Nil))
    val list = value.extractUnchecked(block)
    assertEquals(value.peel(list), block)
    assertEquals(list.length, 4)
    assertEquals(value.arrayLength, Some(4))
