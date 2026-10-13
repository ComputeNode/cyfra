package io.computenode.cyfra.foton

import io.computenode.cyfra.core.{CyfraRuntime, ExpressionProgram, GBufferRegion, GCodec, GProgram, Layout}
import io.computenode.cyfra.core.GBufferRegion.*
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.memory.Focus
import io.computenode.cyfra.foton.GFunction
import io.computenode.cyfra.foton.GFunction.{GFunctionLayout, GFunctionParams}
import izumi.reflect.Tag
import org.lwjgl.BufferUtils

import scala.reflect.ClassTag
import io.computenode.cyfra.core.GCodec.{*, given}
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.GArray.given
import io.computenode.cyfra.core.memory.{BufferRef, GBinding, GBuffer, GUniform, UniformRef}
import io.computenode.cyfra.dsl.direct.GioProgram
import io.computenode.cyfra.dsl.Library.*
import io.computenode.cyfra.dsl.direct.GIO
import io.computenode.cyfra.foton.library.{GArray2D, at}

case class GFunction[G: Value, H: Value, R: Value](underlying: GProgram[GFunctionParams, GFunctionLayout[G, H, R]]):
  given Layout[GFunctionLayout[G, H, R]] = underlying.summonLayout

  def run[GS: ClassTag, HS, RS: ClassTag](input: Array[HS], g: GS)(using
    gCodec: GCodec[G, GS],
    hCodec: GCodec[H, HS],
    rCodec: GCodec[R, RS],
    runtime: CyfraRuntime,
  ): Array[RS] =
    val inTypeSize = typeStride(Value[H])
    val outTypeSize = typeStride(Value[R])
    val uniformStride = typeStride(Value[G])
    val params = GFunctionParams(size = input.size)

    val in = BufferUtils.createByteBuffer(inTypeSize * input.size)
    hCodec.toByteBuffer(in, input)
    val out = BufferUtils.createByteBuffer(outTypeSize * input.size)
    val uniform = BufferUtils.createByteBuffer(uniformStride)
    gCodec.toByteBuffer(uniform, Array(g))

    GBufferRegion
      .allocate[GFunctionLayout[G, H, R]]
      .map: layout =>
        underlying.execute(params, layout)
      .runUnsafe(
        init = GFunctionLayout(in = GBuffer(in), out = GBuffer(input.size), uniform = GUniform(uniform)),
        onDone = layout => layout.out.read(out),
      )
    val resultArray = Array.ofDim[RS](input.size)
    rCodec.fromByteBuffer(out, resultArray)

object GFunction:
  case class GFunctionParams(size: Int)

  case class GFunctionLayout[G: Value, H: Value, R: Value](in: GBuffer[GArray[H]], out: GBuffer[GArray[R]], uniform: GUniform[G])

  object GFunctionLayout:
    given [G: Value, H: Value, R: Value]: Layout[GFunctionLayout[G, H, R]] with
      def fromBindings(bindings: Seq[GBinding[?]]): GFunctionLayout[G, H, R] =
        GFunctionLayout(
          bindings(0).asInstanceOf[GBuffer[GArray[H]]],
          bindings(1).asInstanceOf[GBuffer[GArray[R]]],
          bindings(2).asInstanceOf[GUniform[G]],
        )
      def toBindings(layout: GFunctionLayout[G, H, R]): Seq[GBinding[?]] = Seq(layout.in, layout.out, layout.uniform)
      def layoutRef: GFunctionLayout[G, H, R] = GFunctionLayout(BufferRef[GArray[H]](0), BufferRef[GArray[R]](1), UniformRef[G](2))

  def forEachIndex[G: Value, H: Value, R: Value](fn: (G, Int32, GBuffer[GArray[H]]) => R): GFunction[G, H, R] =
    val program = GioProgram[GFunctionParams, GFunctionLayout[G, H, R]](
      layout = (p: GFunctionParams) => GFunctionLayout[G, H, R](in = GBuffer(p.size), out = GBuffer(p.size), uniform = GUniform()),
      dispatch = (l, p) => StaticDispatch((p.size + 255) / 256, 1, 1),
      workgroupSize = (256, 1, 1),
    ): (layout: GFunctionLayout[G, H, R]) =>
      given vh: Value[GArray[H]] = layout.in.v
      given vr: Value[GArray[R]] = layout.out.v
      val g = GIO.read(layout.uniform)
      val result = fn(g, Int32(invocationIdX), layout.in)
      GIO.write(layout.out.focus(_.at(invocationIdX)), result)

    GFunction(underlying = program)

  def apply[H: Value, R: Value](fn: H => R): GFunction[Int32, H, R] =
    GFunction.forEachIndex[Int32, H, R]((_, index, a) => fn(a.at(index)))

  def apply[G: Value, H: Value, R: Value](fn: (G, H) => R): GFunction[G, H, R] =
    GFunction.forEachIndex[G, H, R]((g, index, a) => fn(g, a.at(index)))

  def from2D[G: Value, H: Value, R: Value](width: Int)(fn: (G, (Int32, Int32), GArray2D[H]) => R): GFunction[G, H, R] =
    GFunction.forEachIndex[G, H, R]((g: G, index: Int32, a: GBuffer[GArray[H]]) =>
      val x: Int32 = index % width
      val y: Int32 = index / width
      val arr = GArray2D(width, a)
      fn(g, (x, y), arr),
    )

  extension [H: Value, R: Value](gf: GFunction[Int32, H, R])
    def run[HS, RS: ClassTag](input: Array[HS])(using hCodec: GCodec[H, HS], rCodec: GCodec[R, RS], runtime: CyfraRuntime): Array[RS] =
      gf.run(input, 0)
