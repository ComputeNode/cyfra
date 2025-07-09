package io.computenode.cyfra.e2e.fs2interop

import io.computenode.cyfra.core.archive.*, mem.*, GMem.fRGBA
import io.computenode.cyfra.dsl.{*, given}, algebra.VectorAlgebra
import io.computenode.cyfra.fs2interop.*, GPipe.*, Bridge.given

import fs2.*

extension (f: fRGBA)
  def neg = (-f._1, -f._2, -f._3, -f._4)
  def scl(s: Float) = (f._1 * s, f._2 * s, f._3 * s, f._4 * s)
  def add(g: fRGBA) = (f._1 + g._1, f._2 + g._2, f._3 + g._3, f._4 + g._4)
  def close(g: fRGBA)(eps: Float): Boolean =
    Math.abs(f._1 - g._1) < eps && Math.abs(f._2 - g._2) < eps && Math.abs(f._3 - g._3) < eps && Math.abs(f._4 - g._4) < eps

class Fs2Tests extends munit.FunSuite:
  given gc: GContext = GContext()

  test("fs2 through gPipeMap, just ints"):
    val inSeq = (0 until 256).toSeq
    val stream = Stream.emits(inSeq)
    val pipe = gPipeMap[Pure, Int32, Int](_ + 1)
    val result = stream.through(pipe).compile.toList
    val expected = inSeq.map(_ + 1)
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp, got $res")

  test("fs2 through gPipeMap, floats and vectors"):
    val inSeq = (0 to 255).map(_.toFloat).toSeq
    val stream = Stream.emits(inSeq)
    val pipe = gPipeMap[Pure, Float32, Vec4[Float32], Float, fRGBA](f => (f, f + 1f, f + 2f, f + 3f))
    val result = stream.through(pipe).compile.toList
    val expected = inSeq.map(f => (f, f + 1f, f + 2f, f + 3f))
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res.close(exp)(0.001f), s"Expected $exp, got $res")

  // legacy tests
  test("fs2 Float stream (legacy)"):
    val inSeq = (0 to 255).map(_.toFloat)
    val inStream = Stream.emits(inSeq)
    val outStream = inStream.gPipeFloat(_ + 1f).toList
    val expected = inStream.map(_ + 1f).toList
    outStream
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")

  test("fs2 Int stream (legacy)"):
    val inSeq = 0 to 255
    val inStream = Stream.emits(inSeq)
    val outStream = inStream.gPipeInt(_ + 1).toList
    val expected = inStream.map(_ + 1).toList
    outStream
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp but got $res")

  test("fs2 Vec4Float stream (legacy)"):
    val k = -2.1f
    val f = (1.2f, 2.3f, 3.4f, 4.5f)
    val v = VectorAlgebra.vec4.tupled(f)

    val inSeq: Seq[fRGBA] = (0 to 1023)
      .map(_.toFloat)
      .grouped(4)
      .map:
        case Seq(a, b, c, d) => (a, b, c, d)
      .toSeq
    val inStream = Stream.emits(inSeq)
    val outStream = inStream.gPipeVec4(vec => (-vec).*(k).+(v)).toList
    val expected = inStream.map(vec => vec.neg.scl(k).add(f)).toList

    outStream
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res.close(exp)(0.001f), s"Expected $exp but got $res")
