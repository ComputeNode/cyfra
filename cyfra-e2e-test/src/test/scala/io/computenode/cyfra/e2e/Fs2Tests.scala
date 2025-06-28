package io.computenode.cyfra.e2e

import io.computenode.cyfra.core.aalegacy.*, mem.*, GMem.fRGBA
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.core.GBufferRegion, GBufferRegion.BufferFromRam
import io.computenode.cyfra.core.{GExecution, GProgram}
import io.computenode.cyfra.dsl.{*, given}, gio.GIO, algebra.VectorAlgebra, buffer.GBuffer
import struct.GStruct, GStruct.Empty, Empty.given

import fs2.{io as fs2io, *}, fs2io.file.{Path, Files}
import cats.effect.IO

import java.nio.{ByteBuffer, file}, file.{Path as nioPath, Files as nioFiles}
import org.lwjgl.BufferUtils
import izumi.reflect.Tag
import scala.reflect.ClassTag
import io.computenode.cyfra.spirv.SpirvTypes.typeStride

object Fs2:
  trait Bridge[CyfraType <: Value: FromExpr: Tag, ScalaType: ClassTag]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[ScalaType]): ByteBuffer
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[ScalaType]): Array[ScalaType]

  given Bridge[Int32, Int]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[Int]): ByteBuffer =
      inBuf.asIntBuffer().put(chunk.toArray[Int]).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Int]): Array[Int] =
      outBuf.asIntBuffer().get(arr).flip
      arr

  given Bridge[Float32, Float]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[Float]): ByteBuffer =
      inBuf.asFloatBuffer().put(chunk.toArray[Float]).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Float]): Array[Float] =
      outBuf.asFloatBuffer().get(arr).flip()
      arr

  given Bridge[Vec4[Float32], fRGBA]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[fRGBA]): ByteBuffer =
      val vecs = chunk.toArray[fRGBA]
      val size = vecs.length
      vecs.foreach:
        case (x, y, z, a) =>
          inBuf.putFloat(x)
          inBuf.putFloat(y)
          inBuf.putFloat(z)
          inBuf.putFloat(a)
      inBuf.flip()
      inBuf

    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[fRGBA]): Array[fRGBA] =
      val res = outBuf.asFloatBuffer()
      for i <- 0 until arr.size do arr(i) = (res.get(), res.get(), res.get(), res.get())
      outBuf.flip()
      arr

  extension [C <: Value, S](buf: ByteBuffer)
    def put(chunk: Chunk[S])(using bridge: Bridge[C, S]): ByteBuffer =
      bridge.toByteBuffer(buf, chunk)
    def get(arr: Array[S])(using bridge: Bridge[C, S]): Array[S] =
      bridge.fromByteBuffer(buf, arr)

  def gPipe[T <: Value: FromExpr: Tag, S: ClassTag](f: T => T)(using GContext, Bridge[T, S]): Pipe[Pure, S, S] =
    (stream: Stream[Pure, S]) =>
      // We need 7 things: params, layout, uniform, result, program, execution, region
      case class Params(inSize: Int)
      case class PUniform() extends GStruct[PUniform]
      case class PLayout(in: GBuffer[T], out: GBuffer[T]) extends Layout
      case class PResult(out: GBuffer[T]) extends Layout

      val params = Params(inSize = 256)
      val typeSize = typeStride(Tag.apply[T])

      val gProg = GProgram[Params, PUniform, PLayout](
        layout = params => PLayout(in = GBuffer[T](params.inSize), out = GBuffer[T](params.inSize)),
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
      val inBuf = BufferUtils.createByteBuffer(params.inSize * typeSize)
      val outBuf = BufferUtils.createByteBuffer(params.inSize * typeSize)

      stream
        .chunkMin(params.inSize)
        .flatMap: chunk =>
          inBuf.put(chunk)
          region.runUnsafe(init = PLayout(in = GBuffer[T](inBuf), out = GBuffer[T](outBuf)), onDone = layout => layout.out.readTo(outBuf))
          Stream.emits(outBuf.get(new Array[S](params.inSize)))

  def gPipe2[C1 <: Value: FromExpr: Tag, C2 <: Value: FromExpr: Tag, S1: ClassTag, S2: ClassTag](
    f: C1 => C2,
  )(using GContext, Bridge[C1, S1], Bridge[C2, S2]): Pipe[Pure, S1, S2] =
    (stream: Stream[Pure, S1]) =>
      case class Params(inSize: Int)
      case class PUniform() extends GStruct[PUniform]
      case class PLayout(in: GBuffer[C1], out: GBuffer[C2]) extends Layout
      case class PResult(out: GBuffer[C2]) extends Layout

      val params = Params(inSize = 256)
      val inTypeSize = typeStride(Tag.apply[C1])
      val outTypeSize = typeStride(Tag.apply[C2])

      val gProg = GProgram[Params, PUniform, PLayout](
        // the sizes are probably wrong
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
          inBuf.put(chunk)
          region.runUnsafe(init = PLayout(in = GBuffer[C1](inBuf), out = GBuffer[C2](outBuf)), onDone = layout => layout.out.readTo(outBuf))
          Stream.emits(outBuf.get(new Array[S2](params.inSize)))

  def gPipeF[F[_], C1 <: Value: FromExpr: Tag, C2 <: Value: FromExpr: Tag, S1: ClassTag, S2: ClassTag](
    f: C1 => C2,
  )(using GContext, Bridge[C1, S1], Bridge[C2, S2]): Pipe[F, S1, S2] =
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
          inBuf.put(chunk)
          region.runUnsafe(init = PLayout(in = GBuffer[C1](inBuf), out = GBuffer[C2](outBuf)), onDone = layout => layout.out.readTo(outBuf))
          Stream.emits(outBuf.get(new Array[S2](params.inSize)))

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

  // needed in tests
  extension (f: fRGBA)
    def neg = (-f._1, -f._2, -f._3, -f._4)
    def scl(s: Float) = (f._1 * s, f._2 * s, f._3 * s, f._4 * s)
    def add(g: fRGBA) = (f._1 + g._1, f._2 + g._2, f._3 + g._3, f._4 + g._4)
    def close(g: fRGBA)(eps: Float): Boolean =
      Math.abs(f._1 - g._1) < eps && Math.abs(f._2 - g._2) < eps && Math.abs(f._3 - g._3) < eps && Math.abs(f._4 - g._4) < eps

class Fs2E2eTest extends munit.FunSuite:
  import Fs2.*
  given gc: GContext = GContext()

  test("fs2 through gPipe"):
    val in = (0 to 255).toSeq
    val stream = Stream.emits(in)
    val pipe = gPipe[Int32, Int](_ + 1)
    val result = stream.through(pipe).compile.toList
    val expected = in.map(_ + 1)
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp, got $res")

  test("fs2 through gPipe2"):
    val in = (0 to 255).map(_.toFloat).toSeq
    val stream = Stream.emits(in)
    val pipe = gPipe2[Float32, Vec4[Float32], Float, fRGBA](f => (f, f + 1f, f + 2f, f + 3f))
    val result = stream.through(pipe).compile.toList
    val expected = in.map(f => (f, f + 1f, f + 2f, f + 3f))
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp, got $res")

  // legacy tests
  test("fs2 Float stream"):
    val inSeq = (0 to 255).map(_.toFloat)
    val inStream = Stream.emits(inSeq)
    val outStream = inStream.gPipeFloat(_ + 1f).toList
    val expected = inStream.map(_ + 1f).toList
    outStream
      .zip(expected)
      .foreach: (res, exp) =>
        assert(Math.abs(res - exp) < 0.001f, s"Expected $exp but got $res")

  test("fs2 Int stream"):
    val inSeq = 0 to 255
    val inStream = Stream.emits(inSeq)
    val outStream = inStream.gPipeInt(_ + 1).toList
    val expected = inStream.map(_ + 1).toList
    outStream
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"Expected $exp but got $res")

  test("fs2 Vec4Float stream"):
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
