package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import Value.FromExpr.fromExpr, control.Scope

class InterpreterE2eTest extends munit.FunSuite:
  test("interpret should not stack overflow"):
    val fakeContext = SimContext(Map(), Map(), SimData())
    val n: Int32 = 0
    val pure = Pure(n)
    var gio = FlatMap(pure, pure)
    for _ <- 0 until 1000000 do gio = FlatMap(pure, gio)
    val result = Interpreter.interpret(gio, fakeContext)
    println("all good, interpret did not stack overflow!")
