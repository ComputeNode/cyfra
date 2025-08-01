package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import Value.FromExpr.fromExpr, control.Scope

class InterpreterE2eTest extends munit.FunSuite:
  test("interpret should not stack overflow"):
    val pure = Pure(0)
    var gio = FlatMap(pure, pure)
    for _ <- 0 until 1000000 do gio = FlatMap(pure, gio)
    val result = Interpreter.interpret(gio, SimContext())
    val res = 0
    val exp = 0
    assert(res == exp, s"Expected $exp, got $res")
