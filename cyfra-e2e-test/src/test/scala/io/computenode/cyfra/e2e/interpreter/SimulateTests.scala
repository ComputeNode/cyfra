package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}, binding.{ReadBuffer, GBuffer}
import Value.FromExpr.fromExpr, control.Scope
import izumi.reflect.Tag

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
    val (result, _) = Simulate.sim(expr, SimContext())
    val expected = 2
    assert(result == expected, s"Expected $expected, got $result")

  test("simulate vec4, scalar, dot, extract scalar"):
    val v1 = ComposeVec4[Float32](1f, 2f, 3f, 4f)
    val (res1, _) = Simulate.sim(v1, SimContext())
    val exp1 = Vector(1f, 2f, 3f, 4f)
    assert(res1 == exp1, s"Expected $exp1, got $res1")

    val i: Int32 = 2
    val expr = ExtractScalar(fromExpr(v1), i)
    val (res, _) = Simulate.sim(expr, SimContext())
    val exp = 3f
    assert(res == exp, s"Expected $exp, got $res")

    val v2 = ScalarProd(fromExpr(v1), -1f)
    val (res2, _) = Simulate.sim(v2, SimContext())
    val exp2 = Vector(-1f, -2f, -3f, -4f)
    assert(res2 == exp2, s"Expected $exp2, got $res2")

    val v3 = ComposeVec4[Float32](-4f, -3f, 2f, 1f)
    val dot = DotProd(fromExpr(v1), fromExpr(v3))
    val (res3a, _) = Simulate.sim(dot, SimContext())
    val res3 = res3a.asInstanceOf[Float]
    val exp3 = 0f
    assert(Math.abs(res3 - exp3) < 0.001f, s"Expected $exp3, got $res3")

  test("simulate bitwise ops"):
    val a: Int32 = 5
    val by: UInt32 = 3
    val aNot = BitwiseNot(a)
    val left = ShiftLeft(fromExpr(aNot), by)
    val right = ShiftRight(fromExpr(aNot), by)
    val and = BitwiseAnd(fromExpr(left), fromExpr(right))
    val or = BitwiseOr(fromExpr(left), fromExpr(right))
    val xor = BitwiseXor(fromExpr(and), fromExpr(or))

    val (res, _) = Simulate.sim(xor, SimContext())
    val exp = ((~5 << 3) & (~5 >> 3)) ^ ((~5 << 3) | (~5 >> 3))
    assert(res == exp, s"Expected $exp, got $res")

  test("simulate should not stack overflow"):
    val a: Int32 = 1
    var sum = Sum(a, a) // 2
    for _ <- 0 until 1000000 do sum = Sum(a, fromExpr(sum))
    val (res, _) = Simulate.sim(sum, SimContext())
    val exp = 1000002
    assert(res == exp, s"Expected $exp, got $res")

  test("simulate ReadBuffer"):
    // We fake a GBuffer with an array
    case class SimGBuffer[T <: Value: Tag: FromExpr]() extends GBuffer[T]
    val buffer = SimGBuffer[Int32]()
    val array = (0 until 1024).toArray[Result]

    val sc = SimContext().addBuffer(buffer, array)

    val expr = ReadBuffer(buffer, 128)
    val (res, newSc) = Simulate.sim(expr, sc)
    val exp = 128
    assert(res == exp, s"Expected $exp, got $res")

    // the context should keep track of the read
    assert(newSc.reads.contains(Read(buffer, 128)), "missing read")
