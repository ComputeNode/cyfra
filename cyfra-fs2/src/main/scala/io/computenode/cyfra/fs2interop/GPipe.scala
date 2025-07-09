package io.computenode.cyfra.fs2interop

import io.computenode.cyfra.core.archive.*, mem.*, GMem.fRGBA
import io.computenode.cyfra.core.{Allocation, layout}, layout.Layout
import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.dsl.{*, given}, gio.GIO, binding.GBuffer
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
  )(using cr: CyfraRuntime, bridge1: Bridge[C1, S1], bridge2: Bridge[C2, S2]): Pipe[F, S1, S2] =
    (stream: Stream[F, S1]) =>
      case class Params(inSize: Int)
      case class PLayout(in: GBuffer[C1], out: GBuffer[C2]) extends Layout
      case class PResult(out: GBuffer[C2]) extends Layout

      val params = Params(inSize = 256)
      val inTypeSize = typeStride(Tag.apply[C1])
      val outTypeSize = typeStride(Tag.apply[C2])

      val gProg = GProgram[Params, PLayout](
        layout = params => PLayout(in = GBuffer[C1](params.inSize), out = GBuffer[C2](params.inSize)),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize, 1, 1)),
      ): layout =>
        val invocId = GIO.invocationId
        val element = GIO.read(layout.in, invocId)
        GIO.write(layout.out, invocId, f(element)) // implicit bug

      val execution = GExecution[Params, PLayout]()
        .addProgram(gProg)(params => Params(params.inSize), layout => PLayout(layout.in, layout.out))

      val region = GBufferRegion
        .allocate[PLayout] // implicit bug
        .map: pLayout =>
          execution.execute(params, pLayout) // implicit bug

      // these are allocated once, reused for many chunks
      val inBuf = BufferUtils.createByteBuffer(params.inSize * inTypeSize)
      val outBuf = BufferUtils.createByteBuffer(params.inSize * outTypeSize)

      stream
        .chunkMin(params.inSize)
        .flatMap: chunk =>
          bridge1.toByteBuffer(inBuf, chunk)
          region.runUnsafe(init = PLayout(in = GBuffer[C1](inBuf), out = GBuffer[C2](outBuf)), onDone = layout => layout.out.read(outBuf)) // implicit bug
          Stream.emits(bridge2.fromByteBuffer(outBuf, new Array[S2](params.inSize)))

  // Syntax sugar for convenient single type version
  def gPipeMap[F[_], C <: Value: FromExpr: Tag, S: ClassTag](f: C => C)(using CyfraRuntime, Bridge[C, S]): Pipe[F, S, S] =
    gPipeMap[F, C, C, S, S](f)

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
