package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}
import Value.FromExpr.fromExpr, control.Scope, binding.{GBuffer, ReadBuffer}
import izumi.reflect.Tag

class SimulateWhenE2eTest extends munit.FunSuite:
  test("simulate when"):
    val expr1 = WhenExpr(
      when = 2 >= 1, // true
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 == 2)), Scope(ConstGB(1 <= 3))),
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val (res1, _) = Simulate.sim(expr1, SimContext())
    val exp1 = 1
    assert(res1 == exp1, s"Expected $exp1, got $res1")

  test("simulate elseWhen first"):
    val expr2 = WhenExpr(
      when = 2 <= 1, // false
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 >= 2)) /*true*/, Scope(ConstGB(1 <= 3))),
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val (res2, _) = Simulate.sim(expr2, SimContext())
    val exp2 = 2
    assert(res2 == exp2, s"Expected $exp2, got $res2")

  test("simulate elseWhen second"):
    val expr3 = WhenExpr(
      when = 2 <= 1, // false
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 == 2)) /*false*/, Scope(ConstGB(1 <= 3))), // true
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val (res3, _) = Simulate.sim(expr3, SimContext())
    val exp3 = 4
    assert(res3 == exp3, s"Expected $exp3, got $res3")

  test("simulate otherwise"):
    val expr4 = WhenExpr(
      when = 2 <= 1, // false
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 == 2)) /*false*/, Scope(ConstGB(1 >= 3))), // false
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val (res4, _) = Simulate.sim(expr4, SimContext())
    val exp4 = 3
    assert(res4 == exp4, s"Expected $exp4, got $res4")

  test("simulate mixed arithmetic, reads and when"):
    case class SimGBuffer[T <: Value: Tag: FromExpr]() extends GBuffer[T]
    val buffer = SimGBuffer[Int32]()
    val array = (128 until 0 by -1).toArray[Result]

    val sc = SimContext().addBuffer(buffer, array)

    val a: Int32 = 32
    val b: Int32 = 64
    val c: Int32 = 4

    val readExpr1 = ReadBuffer(buffer, a) // 96
    val expr1 = Mul(c, fromExpr(readExpr1)) // 4 * 96 = 384

    val readExpr2 = ReadBuffer(buffer, b) // 64
    val expr2 = Sum(c, fromExpr(readExpr2)) // 4 + 64 = 68

    val expr3 = Mod(fromExpr(expr2), 5) // 68 % 5 = 3

    val cond1 = fromExpr(expr1) <= fromExpr(expr2) // 384 <= 68 false
    val cond2 = Equal(fromExpr(expr1), fromExpr(expr2)) // 384 == 68 false
    val cond3 = GreaterThanEqual(fromExpr(expr3), fromExpr(expr2)) // 3 >= 68 false

    val expr = WhenExpr(
      when = cond1, // false
      thenCode = Scope(expr1),
      otherConds = List(Scope(cond2), Scope(cond3)), // false false
      otherCaseCodes = List(Scope(expr1), Scope(expr2)), // 384, 68
      otherwise = Scope(expr3), // 3
    )
    val (res, newSc) = Simulate.sim(expr, sc)
    val exp = 3
    assert(res == exp, s"Expected $exp, got $res")

    // There should be 2 reads in the simulation context
    assert(newSc.reads.contains(ReadBuf(buffer, 32)))
    assert(newSc.reads.contains(ReadBuf(buffer, 64)))
