package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.binding.{Focus, FocusConstant, FocusDynamic, Var}
import io.computenode.cyfra.core.binding.Focus.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.{Tag, TagK}

class FocusTest extends munit.FunSuite:

  private type Inner = (Float32, UInt32)
  private type Struct = (Int32, Inner)

  private def eNa[T: Value](block: ExpressionBlock[?], exp: Expression[T]): T =
    exp.v.extract(block.add(exp))

  private given Value[Inner] = new Value:
    protected def extractUnsafe(ir: ExpressionBlock[Inner]): Inner =
      val a = Expression.Composite[Inner, 0](ir.result, 0)
      val b = Expression.Composite[Inner, 1](ir.result, 1)
      (eNa(ir, a), eNa(ir, b))

    def tag: Tag[Inner] = Tag[Inner]
    def baseTag: Option[TagK[?]] = Some(Tag[Tuple2].asInstanceOf[TagK[?]])
    def composite: List[Value[?]] = List(Value[Float32], Value[UInt32])

  private given Value[Struct] = new Value:
    protected def extractUnsafe(ir: ExpressionBlock[Struct]): Struct =
      val a = Expression.Composite[Struct, 0](ir.result, 0)
      val b = Expression.Composite[Struct, 1](ir.result, 1)
      (eNa(ir, a), b.v.extract(ir.add(b)))

    def tag: Tag[Struct] = Tag[Struct]
    def baseTag: Option[TagK[?]] = Some(Tag[Tuple2].asInstanceOf[TagK[?]])
    def composite: List[Value[?]] = List(Value[Int32], Value[Inner])

  test("focus on nested tuple elements"):
    val v1: Var[Struct] = new Var()

    val result = v1.focus(_._2._1)
    val expected: Focus[Float32] = FocusConstant[Inner, Float32](FocusConstant[Struct, Inner](v1, 2), 1)

    assertEquals(result, expected)

  test("focus with an alternative syntax"):
    val v1: Var[Struct] = new Var()

    val result1 = v1.focus(x => x._2._1)
    val result2 = v1.focus: x =>
      x._2._1
    val expected: Focus[Float32] = FocusConstant[Inner, Float32](FocusConstant[Struct, Inner](v1, 2), 1)

    assertEquals(result1, expected)
    assertEquals(result2, expected)

  test("focus on RuntimeArray with constant index and tuple element"):
    val v2: Var[RuntimeArray[Inner]] = new Var()

    val result = v2.focus(_.at(10)._2)
    val expected: Focus[UInt32] = FocusConstant[Inner, UInt32](FocusConstant[RuntimeArray[Inner], Inner](v2, 10), 2)

    assertEquals(result, expected)

  test("focus on RuntimeArray with dynamic IntegerType index"):
    val v2: Var[RuntimeArray[Inner]] = new Var()
    val i: Int32 = Int32(9)

    val result = v2.focus(_.at(i))
    val expected: Focus[Inner] = FocusDynamic[RuntimeArray[Inner], Inner](v2, i)

    assertEquals(result, expected)
