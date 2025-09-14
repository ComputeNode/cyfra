package io.computenode.cyfra.core.archive

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GCodec, GProgram}
import io.computenode.cyfra.core.GBufferRegion.*
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.archive.GFunction
import io.computenode.cyfra.core.archive.GFunction.{GFunctionLayout, GFunctionParams}
import io.computenode.cyfra.core.layout.{Layout, LayoutBinding, LayoutStruct}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.collections.{GArray, GArray2D}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.*
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import izumi.reflect.Tag
import org.lwjgl.BufferUtils

import scala.reflect.ClassTag
import io.computenode.cyfra.core.GCodec.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct.Empty

case class GFunction[G <: GStruct[G]: {GStructSchema, Tag}, H <: Value: {Tag, FromExpr}, R <: Value: {Tag, FromExpr}](
  underlying: GProgram[GFunctionParams, GFunctionLayout[G, H, R]]
):
  def run[GS : ClassTag, HS, RS : ClassTag] (input: Array[HS], g: GS)(
    using gCodec: GCodec[G, GS],
    hCodec: GCodec[H, HS],
    rCodec: GCodec[R, RS],
    runtime: CyfraRuntime
  ): Array[RS] =
    
    val inTypeSize = typeStride(Tag.apply[H])
    val outTypeSize = typeStride(Tag.apply[R])
    val uniformStride = totalStride(summon[GStructSchema[G]])
    val params = GFunctionParams(size = input.size)
    
    val in = BufferUtils.createByteBuffer(inTypeSize * input.size)
    hCodec.toByteBuffer(in, input)
    val out = BufferUtils.createByteBuffer(outTypeSize * input.size)
    val uniform = BufferUtils.createByteBuffer(uniformStride)
    gCodec.toByteBuffer(uniform, Array(g))
    
    GBufferRegion.allocate[GFunctionLayout[G, H, R]]
      .map: layout =>
        underlying.execute(params, layout)
      .runUnsafe(
        init = GFunctionLayout(
          in = GBuffer[H](in),
          out = GBuffer[R](input.size),
          uniform = GUniform[G](uniform),
        ),
        onDone = layout => 
          layout.out.read(out)
      )
    val resultArray = Array.ofDim[RS](input.size)
    rCodec.fromByteBuffer(out, resultArray)

object GFunction:
  case class GFunctionParams(
    size: Int
  )
  
  case class GFunctionLayout[G <: GStruct[G], H <: Value, R <: Value](
    in: GBuffer[H],
    out: GBuffer[R],
    uniform: GUniform[G]
  ) extends Layout
  
  def forEachIndex[G <: GStruct[G]: {GStructSchema, Tag}, H <: Value: {Tag, FromExpr}, R <: Value: {Tag, FromExpr}](fn: (G, Int32, GBuffer[H]) => R): GFunction[G, H, R] =
    val body = (layout: GFunctionLayout[G, H, R]) => 
      val g = layout.uniform.read
      val result = fn(g, GIO.invocationId, layout.in)
      for
        _ <- layout.out.write(GIO.invocationId, result)
      yield Empty()

    val inTypeSize = typeStride(Tag.apply[H])
    val outTypeSize = typeStride(Tag.apply[R])
      
    GFunction(
      underlying = GProgram.apply[GFunctionParams, GFunctionLayout[G, H, R]](
        layout = (p: GFunctionParams) => GFunctionLayout[G, H, R](
          in = GBuffer[H](p.size),
          out = GBuffer[R](p.size),
          uniform = GUniform[G](),
        ),
        dispatch = (l, p) => StaticDispatch((p.size + 255) / 256, 1, 1),
        workgroupSize = (256, 1, 1),
      )(body)
    )
    
  def apply[H <: Value: {Tag, FromExpr}, R <: Value: {Tag, FromExpr}](fn: H => R): GFunction[GStruct.Empty, H, R] =
    GFunction.forEachIndex[GStruct.Empty, H, R]((g: GStruct.Empty, index: Int32, a: GBuffer[H]) => fn(a.read(index)))
  
  def from2D[G <: GStruct[G]: {GStructSchema, Tag}, H <: Value: {Tag, FromExpr}, R <: Value: {Tag, FromExpr}](
    width: Int,
  )(fn: (G, (Int32, Int32), GArray2D[H]) => R): GFunction[G, H, R] =
    GFunction.forEachIndex[G, H, R]((g: G, index: Int32, a: GBuffer[H]) =>
      val x: Int32 = index mod width
      val y: Int32 = index / width
      val arr = GArray2D(width, a)
      fn(g, (x, y), arr),
    )

  extension [H <: Value: {Tag, FromExpr}, R <: Value: {Tag, FromExpr}](gf: GFunction[GStruct.Empty, H, R])
    def run[HS, RS : ClassTag](input: Array[HS])(
      using hCodec: GCodec[H, HS],
      rCodec: GCodec[R, RS],
      runtime: CyfraRuntime
    ): Array[RS] =
      gf.run(input, GStruct.Empty())