package io.computenode.cyfra.fs2interop

import io.computenode.cyfra.core.aalegacy.*, mem.*, GMem.fRGBA
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.{GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.dsl.{*, given}, gio.GIO, buffer.GBuffer
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import struct.GStruct, GStruct.Empty, Empty.given

import fs2.*
import java.nio.ByteBuffer
import org.lwjgl.BufferUtils
import izumi.reflect.Tag

import scala.reflect.ClassTag

object GPipe:
  def gPipeMap[F[_], C1 <: Value: FromExpr: Tag, C2 <: Value: FromExpr: Tag, S1: ClassTag, S2: ClassTag](
    f: C1 => C2,
  )(using gc: GContext, bridge1: Bridge[C1, S1], bridge2: Bridge[C2, S2]): Pipe[F, S1, S2] =
    (stream: Stream[F, S1]) =>
      case class Params(inSize: Int)
      case class PUniform() extends GStruct[PUniform]
      case class PLayout(in: GBuffer[C1], out: GBuffer[C2]) extends Layout
      case class PResult(out: GBuffer[C2]) extends Layout

      val params = Params(inSize = 256)
      val inTypeSize = typeStride(Tag.apply[C1])
      val outTypeSize = typeStride(Tag.apply[C2])

      val gProg = GProgram[Params, PUniform, PLayout](
        layout = params => PLayout(in = GBuffer[C1](params.inSize), out = GBuffer[C2](params.inSize)),
        uniform = params => PUniform(),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize, 1, 1)),
      ): (layout, uniform) =>
        val invocId = GIO.invocationId
        val element = GIO.read(layout.in, invocId)
        GIO.write(layout.out, invocId, f(element))

      val execution = GExecution
        .build[Params, PLayout, PResult]
        .addProgram(gProg)(mapLayout = layout => PLayout(in = layout.in, out = layout.out), mapParams = params => Params(inSize = params.inSize))
        .compile(layout => PResult(layout.out))

      val region = GBufferRegion
        .allocate[PLayout]
        .map: region =>
          execution.execute(region, params)

      // these are allocated once, reused for many chunks
      val inBuf = BufferUtils.createByteBuffer(params.inSize * inTypeSize)
      val outBuf = BufferUtils.createByteBuffer(params.inSize * outTypeSize)

      stream
        .chunkMin(params.inSize)
        .flatMap: chunk =>
          bridge1.toByteBuffer(inBuf, chunk)
          region.runUnsafe(init = PLayout(in = GBuffer[C1](inBuf), out = GBuffer[C2](outBuf)), onDone = layout => layout.out.readTo(outBuf))
          Stream.emits(bridge2.fromByteBuffer(outBuf, new Array[S2](params.inSize)))

  // Syntax sugar for convenient single type version
  def gPipeMap[F[_], C <: Value: FromExpr: Tag, S: ClassTag](f: C => C)(using GContext, Bridge[C, S]): Pipe[F, S, S] =
    gPipeMap[F, C, C, S, S](f)

  def gPipeFilter[F[_], C <: Value: FromExpr: Tag, S: ClassTag](
    f: C => GBoolean,
  )(using gc: GContext, bridge: Bridge[C, S], boolBridge: Bridge[GBoolean, Boolean]): Pipe[F, S, S] =
    (stream: Stream[F, S]) =>
      case class Params(inSize: Int)
      case class PUniform() extends GStruct[PUniform]
      case class PLayout(in: GBuffer[C], outBool: GBuffer[GBoolean]) extends Layout
      case class PResult(outBool: GBuffer[GBoolean]) extends Layout

      val params = Params(inSize = 256)
      val inTypeSize = typeStride(Tag.apply[C])
      val outTypeSize = typeStride(Tag.apply[GBoolean])

      val gProg = GProgram[Params, PUniform, PLayout](
        layout = params => PLayout(in = GBuffer[C](params.inSize), outBool = GBuffer[GBoolean](params.inSize)),
        uniform = params => PUniform(),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize, 1, 1)),
      ): (layout, uniform) =>
        val invocId = GIO.invocationId
        val element = GIO.read(layout.in, invocId)
        GIO.write(layout.outBool, invocId, f(element))

      val execution = GExecution
        .build[Params, PLayout, PResult]
        .addProgram(gProg)(
          mapLayout = layout => PLayout(in = layout.in, outBool = layout.outBool),
          mapParams = params => Params(inSize = params.inSize),
        )
        .compile(layout => PResult(outBool = layout.outBool))

      val region = GBufferRegion
        .allocate[PLayout]
        .map: region =>
          execution.execute(region, params)

      val inBuf = BufferUtils.createByteBuffer(params.inSize * inTypeSize)
      val outBuf = BufferUtils.createByteBuffer(params.inSize * outTypeSize)

      stream
        .chunkMin(params.inSize)
        .flatMap: chunk =>
          bridge.toByteBuffer(inBuf, chunk)
          region.runUnsafe(
            init = PLayout(in = GBuffer[C](inBuf), outBool = GBuffer[GBoolean](outBuf)),
            onDone = resLayout => resLayout.outBool.readTo(outBuf),
          )
          val bools = new Array[Boolean](params.inSize)
          boolBridge.fromByteBuffer(outBuf, bools)
          val res = chunk.toArray[S].zip(bools)
          ??? // TODO

  // legacy stuff working with GFunction
  extension (stream: Stream[Pure, Float])
    def gPipeFloat(fn: Float32 => Float32)(using GContext): Stream[Pure, Float] =
      val gf: GFunction[Empty, Float32, Float32] = GFunction(fn)
      stream
        .chunkMin(256)
        .flatMap: chunk =>
          val gmem = FloatMem(chunk.toArray)
          val res = gmem.map(gf).asInstanceOf[FloatMem].toArray
          Stream.emits(res)

  extension (stream: Stream[Pure, Int])
    def gPipeInt(fn: Int32 => Int32)(using GContext): Stream[Pure, Int] =
      val gf: GFunction[Empty, Int32, Int32] = GFunction(fn)
      stream
        .chunkMin(256)
        .flatMap: chunk =>
          val gmem = IntMem(chunk.toArray)
          val res = gmem.map(gf).asInstanceOf[IntMem].toArray
          Stream.emits(res)

  extension (stream: Stream[Pure, fRGBA])
    def gPipeVec4(fn: Vec4[Float32] => Vec4[Float32])(using GContext): Stream[Pure, fRGBA] =
      val gf: GFunction[Empty, Vec4[Float32], Vec4[Float32]] = GFunction(fn)
      stream
        .chunkMin(256)
        .flatMap: chunk =>
          val gmem = Vec4FloatMem(chunk.toArray)
          val res = gmem.map(gf).asInstanceOf[Vec4FloatMem].toArray
          Stream.emits(res)
