package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}
import Value.FromExpr.fromExpr, control.Scope, binding.{GBuffer, ReadBuffer}
import izumi.reflect.Tag

class SimulateWhenE2eTest extends munit.FunSuite:
  test("simulate when"):
    given SimData = SimData() // no buffers, reads/writes here
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

    val expr = WhenExpr(
      when = 2 >= 1, // true
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 == 2)), Scope(ConstGB(1 <= 3))),
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val SimContext(res, _, _) = Simulate.sim(expr, startingRecords)
    val exp = 1
    assert(res(0) == exp, s"Expected $exp, got ${res(0)}")

  test("simulate elseWhen first"):
    given SimData = SimData() // no buffers, reads/writes here
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

    val expr = WhenExpr(
      when = 2 <= 1, // false
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 >= 2)) /*true*/, Scope(ConstGB(1 <= 3))),
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val SimContext(res, _, _) = Simulate.sim(expr, startingRecords)
    val exp = 2
    assert(res(0) == exp, s"Expected $exp, got ${res(0)}")

  test("simulate elseWhen second"):
    given SimData = SimData() // no buffers, reads/writes here
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

    val expr = WhenExpr(
      when = 2 <= 1, // false
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 == 2)) /*false*/, Scope(ConstGB(1 <= 3))), // true
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val SimContext(res, _, _) = Simulate.sim(expr, startingRecords)
    val exp = 4
    assert(res(0) == exp, s"Expected $exp, got $res")

  test("simulate otherwise"):
    given SimData = SimData() // no buffers, reads/writes here
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

    val expr = WhenExpr(
      when = 2 <= 1, // false
      thenCode = Scope(ConstInt32(1)),
      otherConds = List(Scope(ConstGB(3 == 2)) /*false*/, Scope(ConstGB(1 >= 3))), // false
      otherCaseCodes = List(Scope(ConstInt32(2)), Scope(ConstInt32(4))),
      otherwise = Scope(ConstInt32(3)),
    )
    val SimContext(res, _, _) = Simulate.sim(expr, startingRecords)
    val exp = 3
    assert(res(0) == exp, s"Expected $exp, got $res")

  test("simulate mixed arithmetic, buffer reads and when"):
    case class SimGBuffer[T <: Value: Tag: FromExpr]() extends GBuffer[T]
    val buffer = SimGBuffer[Int32]()
    val array = (0 until 3).toArray[Result]

    given SimData = SimData().addBuffer(buffer, array)
    val startingRecords = Map(0 -> Record(), 1 -> Record(), 2 -> Record()) // running 3 invocations

    val a: Int32 = 4
    val invocId = InvocationId
    val readExpr = ReadBuffer(buffer, fromExpr(invocId)) // 0,1,2

    val expr1 = Mul(a, fromExpr(readExpr)) // 4*0 = 0, 4*1 = 4, 4*2 = 8
    val expr2 = Sum(a, fromExpr(expr1)) // 4+0 = 4, 4+4 = 8, 4+8 = 12
    val expr3 = Mod(fromExpr(expr2), 5) // 4%5 = 4, 8%5 = 3, 12%5 = 2

    val cond1 = fromExpr(expr1) <= fromExpr(expr3)
    val cond2 = Equal(fromExpr(expr3), fromExpr(readExpr))

    // invoc 0 enters when, invoc2 enters elseWhen, invoc1 enters otherwise
    val expr = WhenExpr(
      when = cond1, // true false false
      thenCode = Scope(expr1), // 0 _ _
      otherConds = List(Scope(cond2)), // _ false true
      otherCaseCodes = List(Scope(expr2)), // _ _ 12
      otherwise = Scope(expr3), // _ 3 _
    )
    val SimContext(res, records, _) = Simulate.sim(expr, startingRecords)
    val exp = Map(0 -> 0, 1 -> 3, 2 -> 12)
    assert(res == exp, s"Expected $exp, got $res")
