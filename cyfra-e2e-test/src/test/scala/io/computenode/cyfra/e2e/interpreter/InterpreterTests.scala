package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}
import Value.FromExpr.fromExpr, control.Scope

class InterpreterE2eTest extends munit.FunSuite:
  test("stub"):
    val res = 0
    val exp = 0
    assert(res == exp, s"Expected $exp, got $res")
