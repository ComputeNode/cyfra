package io.computenode.cyfra.e2e.interpreter

import io.computenode.cyfra.interpreter.*, Result.*
import io.computenode.cyfra.dsl.{*, given}, binding.{ReadBuffer, GBuffer}
import Value.FromExpr.fromExpr, control.Scope
import izumi.reflect.Tag

class SimulateE2eTest extends munit.FunSuite:
  test("simulate binary operation arithmetic, record cache"):
    given SimContext = SimContext() // no buffers, reads/writes here
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

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

    val SimRes(results, records, _) = Simulate.sim(expr, startingRecords)
    val expected = 2
    assert(results(0) == expected, s"Expected $expected, got $results")

    // records cache should have kept track of intermediate expression results correctly
    // 0 -> 1  a
    // 1 -> 2  b
    // 2 -> 3  c
    // 3 -> 4  d
    // 4 -> 5  e
    // 5 -> 6  f
    // 6 -> -1 e1
    // 7 -> 2  e2
    // 8 -> 12 e3
    // 9 -> 3  e4
    // 10 -> 2 expr
    val map = Map(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 6 -> -1, 7 -> 2, 8 -> 12, 9 -> 3, 10 -> 2)
    assert(records(0).cache == map)

  test("simulate Vec4, scalar, dot, extract scalar"):
    given SimContext = SimContext() // no buffers, reads/writes here
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

    val v1 = ComposeVec4[Float32](1f, 2f, 3f, 4f)
    val SimRes(res1, records1, _) = Simulate.sim(v1, startingRecords)
    val exp1 = Vector(1f, 2f, 3f, 4f)
    assert(res1(0) == exp1, s"Expected $exp1, got ${res1(0)}")

    val i: Int32 = 2
    val expr = ExtractScalar(fromExpr(v1), i)
    val SimRes(res2, records2, _) = Simulate.sim(expr, records1)
    val exp2 = 3f
    assert(res2(0) == exp2, s"Expected $exp2, got ${res2(0)}")

    val v2 = ScalarProd(fromExpr(v1), -1f)
    val SimRes(res3, records3, _) = Simulate.sim(v2, records2)
    val exp3 = Vector(-1f, -2f, -3f, -4f)
    assert(res3(0) == exp3, s"Expected $exp3, got ${res3(0)}")

    val v3 = ComposeVec4[Float32](-4f, -3f, 2f, 1f)
    val dot = DotProd(fromExpr(v1), fromExpr(v3))
    val SimRes(results, records4, _) = Simulate.sim(dot, records3)
    val exp4 = 0f
    val res4 = results(0).asInstanceOf[Float]
    assert(Math.abs(res4 - exp4) < 0.001f, s"Expected $exp4, got $res4")

  test("simulate bitwise ops"):
    given SimContext = SimContext() // no buffers, reads/writes here
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

    val a: Int32 = 5
    val by: UInt32 = 3
    val aNot = BitwiseNot(a)
    val left = ShiftLeft(fromExpr(aNot), by)
    val right = ShiftRight(fromExpr(aNot), by)
    val and = BitwiseAnd(fromExpr(left), fromExpr(right))
    val or = BitwiseOr(fromExpr(left), fromExpr(right))
    val xor = BitwiseXor(fromExpr(and), fromExpr(or))

    val SimRes(res, records1, _) = Simulate.sim(xor, startingRecords)
    val exp = ((~5 << 3) & (~5 >> 3)) ^ ((~5 << 3) | (~5 >> 3))
    assert(res(0) == exp, s"Expected $exp, got ${res(0)}")

  test("simulate should not stack overflow"):
    given SimContext = SimContext() // no buffers, reads/writes here
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

    val a: Int32 = 1
    var sum = Sum(a, a) // 2
    for _ <- 0 until 1000000 do sum = Sum(a, fromExpr(sum))
    val SimRes(res, records, _) = Simulate.sim(sum, startingRecords)
    val exp = 1000002
    assert(res(0) == exp, s"Expected $exp, got ${res(0)}")

  test("simulate ReadBuffer"):
    // We fake a GBuffer with an array
    case class SimGBuffer[T <: Value: Tag: FromExpr]() extends GBuffer[T]
    val buffer = SimGBuffer[Int32]()
    val array = (0 until 1024).toArray[Result]

    given SimContext = SimContext().addBuffer(buffer, array)
    val startingRecords = Map(0 -> Record()) // running with only 1 invocation

    val expr = ReadBuffer(buffer, 128)
    val SimRes(res, records, _) = Simulate.sim(expr, startingRecords)
    val exp = 128
    assert(res(0) == exp, s"Expected $exp, got $res")

    // the records should keep track of the read
    val read = ReadBuf(expr.treeid, buffer, 128, 128) // 128 has treeid 0, so expr has treeid 1
    assert(records(0).reads.contains(read), "missing read")
