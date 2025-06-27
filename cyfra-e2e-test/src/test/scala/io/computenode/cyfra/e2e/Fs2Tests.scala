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
  type Cyfra[X] = X match
    case Int   => Int32
    case Float => Float32
    case fRGBA => Vec4[Float32]
  type Scala[X] = X match
    case Int32         => Int
    case Float32       => Float
    case Vec4[Float32] => fRGBA

  trait Bridge[CyfraType <: Value: FromExpr: Tag, ScalaType: ClassTag]:
    def toByteBuffer(buf: ByteBuffer, chunk: Chunk[ScalaType]): ByteBuffer
    def fromByteBuffer(buf: ByteBuffer, arr: Array[ScalaType]): ByteBuffer

  given Bridge[Int32, Int]:
    def toByteBuffer(buf: ByteBuffer, chunk: Chunk[Int]): ByteBuffer = ???
    def fromByteBuffer(buf: ByteBuffer, arr: Array[Int]): ByteBuffer = ???

  given Bridge[Float32, Float]:
    def toByteBuffer(buf: ByteBuffer, chunk: Chunk[Float]): ByteBuffer = ???
    def fromByteBuffer(buf: ByteBuffer, arr: Array[Float]): ByteBuffer = ???

  extension [C <: Value, S](buf: ByteBuffer)
    def put(chunk: Chunk[S])(using bridge: Bridge[C, S]): ByteBuffer =
      bridge.toByteBuffer(buf, chunk)
    def get(arr: Array[S])(using bridge: Bridge[C, S]) =
      bridge.fromByteBuffer(buf, arr)

  def gPipePoly[T <: Value: FromExpr: Tag, S: ClassTag](f: T => T)(using GContext, Bridge[T, S]): Pipe[Pure, S, S] =
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
          inBuf.put(chunk).flip()
          region.runUnsafe(init = PLayout(in = GBuffer[T](inBuf), out = GBuffer[T](outBuf)), onDone = layout => layout.out.readTo(outBuf))
          val resArr = new Array[S](params.inSize)
          outBuf.get(resArr).flip()
          Stream.emits(resArr)

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

class Fs2E2eTest extends munit.FunSuite:
  import Fs2.*
  given gc: GContext = GContext()

  test("testing stuff"):
    // We need 7 things: params, layout, uniform, result, program, execution, region
    case class Params(inSize: Int)
    case class PLayout(in: GBuffer[Int32], out: GBuffer[Int32]) extends Layout
    case class PUniform(factor: Int32 = 0) extends GStruct[PUniform]
    case class PResult(out: GBuffer[Int32]) extends Layout

    val params = Params(inSize = 1024)

    val prog = GProgram[Params, PUniform, PLayout](
      layout = params => PLayout(in = GBuffer[Int32](params.inSize), out = GBuffer[Int32](params.inSize)),
      uniform = params => PUniform(),
      dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize, 1, 1)),
    ): (layout, uniform) =>
      val invocId = GIO.invocationId
      val element = GIO.read(layout.in, invocId)
      GIO.write(layout.out, invocId, element)

    val execution = GExecution
      .build[Params, PLayout, PResult]
      .addProgram(prog)(mapLayout = layout => PLayout(in = layout.in, out = layout.out), mapParams = params => Params(inSize = params.inSize))
      .compile(layout => PResult(layout.out))

    val region = GBufferRegion
      .allocate[PLayout]
      .map: region =>
        execution.execute(region, params)

    // file has 256 * 4096 random bytes, interpreted as 256 * 1024 random Ints
    val arrCount = 256 // 256 arrays
    val arrSize = 1024 // 1024 Ints each
    val batchSize = 16 // 16 arrays at a time
    val intSize = 4
    val chunkSizeMax = arrSize * batchSize
    val sizeInBytesMax = chunkSizeMax * intSize

    val file = nioPath.of("src/test/resources/bytes")
    val bytes = nioFiles.readAllBytes(file)
    val buf = ByteBuffer.wrap(bytes).asIntBuffer()
    val bigArr = new Array[Int](arrCount * arrSize)
    buf.get(bigArr)

    // to feed into fs2
    val arrays = bigArr.grouped(arrSize).toSeq // 256 arrays
    val stream = Stream.emits(arrays) // a stream of 256 Array[Int]s

    // fs2 land -> cyfra -> back to fs2 land
    val inBuf = BufferUtils.createByteBuffer(sizeInBytesMax)
    val outBuf = BufferUtils.createByteBuffer(sizeInBytesMax)

    val res = stream
      .chunkN(batchSize)
      .map: chunk =>
        val intArr = chunk.toArray.flatten // 16 arrays, 16 * 1024 Ints
        inBuf.asIntBuffer().put(intArr).flip()

        region.runUnsafe(init = PLayout(in = GBuffer[Int32](inBuf), out = GBuffer[Int32](outBuf)), onDone = layout => layout.out.readTo(outBuf))

        val resArr = new Array[Int](chunkSizeMax)
        outBuf.asIntBuffer().get(resArr).flip()
        resArr

    val result = res.compile.toList.flatten
    val expected = arrays.flatten
    result
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res == exp, s"expected $exp, got $res")

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

    extension (f: fRGBA)
      def neg = (-f._1, -f._2, -f._3, -f._4)
      def scl(s: Float) = (f._1 * s, f._2 * s, f._3 * s, f._4 * s)
      def add(g: fRGBA) = (f._1 + g._1, f._2 + g._2, f._3 + g._3, f._4 + g._4)
      def close(g: fRGBA)(eps: Float): Boolean =
        Math.abs(f._1 - g._1) < eps && Math.abs(f._2 - g._2) < eps && Math.abs(f._3 - g._3) < eps && Math.abs(f._4 - g._4) < eps

    val expected = inStream.map(vec => vec.neg.scl(k).add(f)).toList

    outStream
      .zip(expected)
      .foreach: (res, exp) =>
        assert(res.close(exp)(0.001f), s"Expected $exp but got $res")
