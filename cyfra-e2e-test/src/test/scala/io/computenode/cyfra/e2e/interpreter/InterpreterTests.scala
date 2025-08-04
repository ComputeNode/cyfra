package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}
import binding.*, Value.*, gio.GIO, GIO.*
import FromExpr.fromExpr, control.Scope
import izumi.reflect.Tag

class InterpreterE2eTest extends munit.FunSuite:
  test("interpret should not stack overflow"):
    val fakeContext = SimContext(Map(), Map(), SimData())
    val n: Int32 = 0
    val pure = Pure(n)
    var gio = FlatMap(pure, pure)
    for _ <- 0 until 1000000 do gio = FlatMap(pure, gio)
    val result = Interpreter.interpret(gio, fakeContext)
    println("all good, interpret did not stack overflow!")

  test("interpret mixed arithmetic, buffer reads/writes, uniform reads/writes, and when"):
    case class SimGBuffer[T <: Value: Tag: FromExpr]() extends GBuffer[T]
    val buffer = SimGBuffer[Int32]()
    val array = (0 until 3).toArray[Result]

    case class SimGUniform[T <: Value: Tag: FromExpr]() extends GUniform[T]
    val uniform = SimGUniform[Int32]()
    val uniValue = 4

    val data = SimData().addBuffer(buffer, array).addUniform(uniform, uniValue)
    val startingRecords = Map(0 -> Record(), 1 -> Record(), 2 -> Record()) // running 3 invocations
    val startingSc = SimContext(records = startingRecords, data = data)

    val a = ReadUniform(uniform) // 4
    val invocId = InvocationId
    val readExpr = ReadBuffer(buffer, fromExpr(invocId)) // 0,1,2

    val expr1 = Mul(fromExpr(a), fromExpr(readExpr)) // 4*0 = 0, 4*1 = 4, 4*2 = 8
    val expr2 = Sum(fromExpr(a), fromExpr(expr1)) // 4+0 = 4, 4+4 = 8, 4+8 = 12
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

    val writeBufGIO = WriteBuffer(buffer, fromExpr(invocId), fromExpr(expr))
    val writeUniGIO = WriteUniform(uniform, fromExpr(expr))
    val gio = FlatMap(writeBufGIO, writeUniGIO)

    val sc = Interpreter.interpret(gio, startingSc)
    println(sc) // TODO not sure what/how to test for now.
