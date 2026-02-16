package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.memory.{Focus, FocusConstant, FocusDynamic, LocalVariable, Variable}
import io.computenode.cyfra.core.memory.Focus.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.{Tag, Tag}

class FocusTest extends munit.FunSuite:

  private type Inner = (Float32, UInt32)
  private type Struct = (Int32, Inner, UInt32)

  test("value derives correctly"):
    val v = Value[Struct]
    assertEquals(v.bottomComposite, v)
    assert(v.baseTag.get =:= Tag[Tuple])

    val composites = v.composite.map(_.tag)
    assertEquals(composites.size, 3)
    val List(t1, t2, t3) = composites
    assert(t1 =:= Tag[Int32])
    assert(t2 =:= Tag[Inner])
    assert(t3 =:= Tag[UInt32])

  test("focus on nested tuple elements"):
    val v1: LocalVariable[Struct] = new LocalVariable()

    val result = v1.focus(_._2._1)
    val expected: Focus[Float32] = FocusConstant[Inner, Float32](FocusConstant[Struct, Inner](v1, 2), 1)

    assertEquals(result, expected)

  test("focus with an alternative syntax"):
    val v1: LocalVariable[Struct] = new LocalVariable()

    val result1 = v1.focus(x => x._2._1)
    val result2 = v1.focus: x =>
      x._2._1
    val expected: Focus[Float32] = FocusConstant[Inner, Float32](FocusConstant[Struct, Inner](v1, 2), 1)

    assertEquals(result1, expected)
    assertEquals(result2, expected)

  test("focus on RuntimeArray with constant index and tuple element"):
    val v2: LocalVariable[RuntimeArray[Inner]] = new LocalVariable()

    val result = v2.focus(_.at(10)._2)
    val expected: Focus[UInt32] = FocusConstant[Inner, UInt32](FocusConstant[RuntimeArray[Inner], Inner](v2, 10), 2)

    assertEquals(result, expected)

  test("focus on RuntimeArray with dynamic IntegerType index"):
    val v2: LocalVariable[RuntimeArray[Inner]] = new LocalVariable()
    val i: Int32 = Int32(9)

    val result = v2.focus(_.at(i))
    val expected: Focus[Inner] = FocusDynamic[RuntimeArray[Inner], Inner](v2, i)

    assertEquals(result, expected)

  // Case class field access tests
  case class Point(x: Float32, y: Float32)
  case class Line(start: Point, end: Point)

  test("focus on case class field"):
    val v: LocalVariable[Point] = new LocalVariable()

    val result = v.focus(_.x)
    val expected: Focus[Float32] = FocusConstant[Point, Float32](v, 1)

    assertEquals(result, expected)

  test("focus on second case class field"):
    val v: LocalVariable[Point] = new LocalVariable()

    val result = v.focus(_.y)
    val expected: Focus[Float32] = FocusConstant[Point, Float32](v, 2)

    assertEquals(result, expected)

  test("focus on nested case class fields"):
    val v: LocalVariable[Line] = new LocalVariable()

    val result = v.focus(_.end.x)
    val expected: Focus[Float32] = FocusConstant[Point, Float32](FocusConstant[Line, Point](v, 2), 1)

    assertEquals(result, expected)

  test("focus on case class in RuntimeArray"):
    val v: LocalVariable[RuntimeArray[Point]] = new LocalVariable()

    val result = v.focus(_.at(5).y)
    val expected: Focus[Float32] = FocusConstant[Point, Float32](FocusConstant[RuntimeArray[Point], Point](v, 5), 2)

    assertEquals(result, expected)
