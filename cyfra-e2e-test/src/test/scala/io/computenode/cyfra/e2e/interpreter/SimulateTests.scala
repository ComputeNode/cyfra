package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}
import Value.FromExpr.fromExpr, control.Scope

class SimulateE2eTest extends munit.FunSuite:
  test("simulate binary operation arithmetic"):
    val a: Int32 = 1
    val b: Int32 = 2
    val c: Int32 = 3
    val d: Int32 = 4
    val e: Int32 = 5
    val f: Int32 = 6
    val e1 = Diff(a, b)
    val e2 = Sum(fromExpr(e1), c)
    val e3 = Mul(f, fromExpr(e2))
    val e4 = Div(fromExpr(e3), d)
    val expr = Mod(e, fromExpr(e4)) // 5 % ((6 * ((1 - 2) + 3)) / 4)
    val result = Simulate.sim(expr)
    val expected = 2
    assert(result == expected, s"Expected $expected, got $result")

  test("simulate vec4, scalar, dot"):
    val v1 = ComposeVec4[Float32](1f, 2f, 3f, 4f)
    val res1 = Simulate.sim(v1)
    val exp1 = Vector(1f, 2f, 3f, 4f)
    assert(res1 == exp1, s"Expected $exp1, got $res1")

    val v2 = ScalarProd(fromExpr(v1), -1f)
    val res2 = Simulate.sim(v2)
    val exp2 = Vector(-1f, -2f, -3f, -4f)
    assert(res2 == exp2, s"Expected $exp2, got $res2")

    val v3 = ComposeVec4[Float32](-4f, -3f, 2f, 1f)
    val dot = DotProd(fromExpr(v1), fromExpr(v3))
    val res3 = Simulate.sim(dot).asInstanceOf[Float]
    val exp3 = 0f
    assert(Math.abs(res3 - exp3) < 0.001f, s"Expected $exp3, got $res3")

  test("when test"):
    val expr = WhenExpr(
      when = 2 <= 1,
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 == 2)), Scope(ConstGB(1 >= 3))),
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val res = Simulate.sim(expr)
    val exp = 3
    assert(res == exp, s"Expected $exp, got $res")
