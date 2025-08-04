package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}, binding.{ReadBuffer, GBuffer}
import Value.FromExpr.fromExpr, control.Scope
import izumi.reflect.Tag

class SimulateE2eTest extends munit.FunSuite:
  test("simulate binary operation arithmetic, record cache"):
    val startingSc = SimContext(records = Map(0 -> Record())) // running with only 1 invocation

    val a: Int32 = 1
    val b: Int32 = 2
    val c: Int32 = 3
    val d: Int32 = 4
    val e: Int32 = 5
    val f: Int32 = 6
    val e1 = Diff(a, b) // -1
    val e2 = Sum(fromExpr(e1), c) // 2
    val e3 = Mul(f, fromExpr(e2)) // 12
    val e4 = Div(fromExpr(e3), d) // 3
    val expr = Mod(e, fromExpr(e4)) // 5 % ((6 * ((1 - 2) + 3)) / 4)

    val SimContext(results, records, _, _) = Simulate.sim(expr, startingSc)
    val expected = 2
    assert(results(0) == expected, s"Expected $expected, got $results")

    // records cache should have kept track of intermediate expression results correctly
    val exp = Map(
      a.treeid -> 1,
      b.treeid -> 2,
      c.treeid -> 3,
      d.treeid -> 4,
      e.treeid -> 5,
      f.treeid -> 6,
      e1.treeid -> -1,
      e2.treeid -> 2,
      e3.treeid -> 12,
      e4.treeid -> 3,
      expr.treeid -> 2,
    )
    val res = records(0).cache
    assert(res == exp, s"Expected $exp, got $res")

  test("simulate Vec4, scalar, dot, extract scalar"):
    val startingSc = SimContext(records = Map(0 -> Record())) // running with only 1 invocation

    val v1 = ComposeVec4[Float32](1f, 2f, 3f, 4f)
    val sc1 = Simulate.sim(v1, startingSc)
    val exp1 = Vector(1f, 2f, 3f, 4f)
    val res1 = sc1.results(0)
    assert(res1 == exp1, s"Expected $exp1, got $res1")

    val i: Int32 = 2
    val expr = ExtractScalar(fromExpr(v1), i)
    val sc2 = Simulate.sim(expr, sc1)
    val exp2 = 3f
    val res2 = sc2.results(0)
    assert(res2 == exp2, s"Expected $exp2, got $res2")

    val v2 = ScalarProd(fromExpr(v1), -1f)
    val sc3 = Simulate.sim(v2, sc2)
    val exp3 = Vector(-1f, -2f, -3f, -4f)
    val res3 = sc3.results(0)
    assert(res3 == exp3, s"Expected $exp3, got $res3")

    val v3 = ComposeVec4[Float32](-4f, -3f, 2f, 1f)
    val dot = DotProd(fromExpr(v1), fromExpr(v3))
    val SimContext(results, _, _, _) = Simulate.sim(dot, sc3)
    val exp4 = 0f
    val res4 = results(0).asInstanceOf[Float]
    assert(Math.abs(res4 - exp4) < 0.001f, s"Expected $exp4, got $res4")

  test("simulate bitwise ops"):
    val startingSc = SimContext(records = Map(0 -> Record())) // running with only 1 invocation

    val a: Int32 = 5
    val by: UInt32 = 3
    val aNot = BitwiseNot(a)
    val left = ShiftLeft(fromExpr(aNot), by)
    val right = ShiftRight(fromExpr(aNot), by)
    val and = BitwiseAnd(fromExpr(left), fromExpr(right))
    val or = BitwiseOr(fromExpr(left), fromExpr(right))
    val xor = BitwiseXor(fromExpr(and), fromExpr(or))

    val SimContext(res, _, _, _) = Simulate.sim(xor, startingSc)
    val exp = ((~5 << 3) & (~5 >> 3)) ^ ((~5 << 3) | (~5 >> 3))
    assert(res(0) == exp, s"Expected $exp, got ${res(0)}")

  test("simulate should not stack overflow"):
    val startingSc = SimContext(records = Map(0 -> Record())) // running with only 1 invocation

    val a: Int32 = 1
    var sum = Sum(a, a) // 2
    for _ <- 0 until 1000000 do sum = Sum(a, fromExpr(sum))
    val SimContext(res, _, _, _) = Simulate.sim(sum, startingSc)
    val exp = 1000002
    assert(res(0) == exp, s"Expected $exp, got ${res(0)}")

  test("simulate ReadBuffer"):
    // We fake a GBuffer with an array
    case class SimGBuffer[T <: Value: Tag: FromExpr]() extends GBuffer[T]
    val buffer = SimGBuffer[Int32]()
    val array = (0 until 1024).toArray[Result]

    val data = SimData().addBuffer(buffer, array)
    val startingSc = SimContext(records = Map(0 -> Record()), data = data) // running with only 1 invocation

    val expr = ReadBuffer(buffer, 128)
    val SimContext(res, records, _, _) = Simulate.sim(expr, startingSc)
    val exp = 128
    assert(res(0) == exp, s"Expected $exp, got $res")

    // the records should keep track of the read
    val read = ReadBuf(expr.treeid, buffer, 128, 128) // 128 has treeid 0, so expr has treeid 1
    assert(records(0).reads.contains(read), "missing read")
