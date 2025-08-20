package io.computenode.cyfra.e2e.fs2interop

import io.computenode.cyfra.core.archive.*
import io.computenode.cyfra.dsl.{*, given}
import algebra.VectorAlgebra
import io.computenode.cyfra.fs2interop.*
import io.computenode.cyfra.core.CyfraRuntime
import io.computenode.cyfra.runtime.VkCyfraRuntime
import fs2.{io as fs2io, *}
import _root_.io.computenode.cyfra.spirvtools.{SpirvCross, SpirvDisassembler, SpirvToolsRunner}
import _root_.io.computenode.cyfra.spirvtools.SpirvTool.ToFile

import java.nio.file.Paths

extension (f: fRGBA)
  def neg = (-f._1, -f._2, -f._3, -f._4)
  def scl(s: Float) = (f._1 * s, f._2 * s, f._3 * s, f._4 * s)
  def add(g: fRGBA) = (f._1 + g._1, f._2 + g._2, f._3 + g._3, f._4 + g._4)
  def close(g: fRGBA)(eps: Float): Boolean =
    Math.abs(f._1 - g._1) < eps && Math.abs(f._2 - g._2) < eps && Math.abs(f._3 - g._3) < eps && Math.abs(f._4 - g._4) < eps

class Fs2Tests extends munit.FunSuite:
  given cr: VkCyfraRuntime = VkCyfraRuntime(spirvToolsRunner =
    SpirvToolsRunner(
      crossCompilation = SpirvCross.Enable(toolOutput = ToFile(Paths.get("output/optimized.glsl"))),
      disassembler = SpirvDisassembler.Enable(toolOutput = ToFile(Paths.get("output/disassembled.spv"))),
    ),
  )

  override def afterAll(): Unit =
    // cr.close()
    super.afterAll()

  test("fs2 through GPipe map, just ints"):
    val inSeq = (0 until 256).toSeq
    val stream = Stream.emits(inSeq)
    val pipe = GPipe.map[Pure, Int32, Int](_ + 1)
    val result = stream.through(pipe).compile.toList
    val expected = inSeq.map(_ + 1)
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp, got $res")

  test("fs2 through GPipe map, floats and vectors"):
    val n = 16
    val inSeq = (0 until n * 256).map(_.toFloat)
    val stream = Stream.emits(inSeq)
    val pipe = GPipe.map[Pure, Float32, Vec4[Float32], Float, fRGBA](f => (f, f + 1f, f + 2f, f + 3f))
    val result = stream.through(pipe).compile.toList
    val expected = inSeq.map(f => (f, f + 1f, f + 2f, f + 3f))
    println("DONE!")
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res.close(exp)(0.001f), s"Expected $exp, got $res")

  test("fs2 through GPipe filter, just ints"):
    val n = 16
    val inSeq = 0 until n * 256
    val stream = Stream.emits(inSeq)
    val pipe = GPipe.filter[Pure, Int32, Int](_.mod(7) === 0)
    val result = stream.through(pipe).compile.toList
    val expected = inSeq.filter(_ % 7 == 0)
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp, got $res")
