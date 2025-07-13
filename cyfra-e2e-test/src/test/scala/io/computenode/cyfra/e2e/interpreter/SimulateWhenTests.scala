package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}
import Value.FromExpr.fromExpr, control.Scope

class SimulateWhenE2eTest extends munit.FunSuite:
  test("simulate when"):
    val expr1 = WhenExpr(
      when = 2 >= 1, // true
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 == 2)), Scope(ConstGB(1 <= 3))),
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val res1 = Simulate.sim(expr1)
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
    val res2 = Simulate.sim(expr2)
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
    val res3 = Simulate.sim(expr3)
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
    val res4 = Simulate.sim(expr4)
    val exp4 = 3
    assert(res4 == exp4, s"Expected $exp4, got $res4")
