package io.computenode.cyfra.fs2interop

import io.computenode.cyfra.core.archive.*, mem.*, GMem.fRGBA
import io.computenode.cyfra.core.{Allocation, layout}, layout.Layout
import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.dsl.{*, given}, gio.GIO, binding.{GBuffer, GUniform, GBinding}
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
        val element = GIO.read[C1](layout.in, invocId)
        GIO.write[C2](layout.out, invocId, f(element))

      val execution = GExecution[Params, PLayout]()
        .addProgram(gProg)(params => Params(params.inSize), layout => PLayout(layout.in, layout.out))

      val region = GBufferRegion
        .allocate[PLayout]
        .map: pLayout =>
          execution.execute(params, pLayout)

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

  // https://developer.nvidia.com/gpugems/gpugems3/part-vi-gpu-computing/chapter-39-parallel-prefix-sum-scan-cuda
  // Prefix Sum and Stream Compaction
  //            11
  //  012345678901  index
  // [abcdefghijkl] starting collection
  // [tfftfftttftf] convert to booleans
  // [100100111010] integer equivalent
  // [111222345566] prefixsum, last number tells us the size of filtered collection
  // [x..x..xxx.x.] take the ones that are 1 bigger than previous.
  // [adhijl]       compact the collection
  //  012345        (prefixsum result - 1) are the new indices in compacted collection
  def gPipeFilter[F[_], C <: Value: FromExpr: Tag, S: ClassTag](pred: C => GBoolean)(using cr: CyfraRuntime, bridge: Bridge[C, S]): Pipe[F, S, S] =
    (stream: Stream[F, S]) =>
      case class Params(inSize: Int, intervalSize: Int)
      case class PredLayout(cIn: GBuffer[C], boolOut: GBuffer[Int32]) extends Layout
      case class ScanArgs(intervalSize: Int32) extends GStruct[ScanArgs]
      case class ScanLayout(intIn: GBuffer[Int32], intOut: GBuffer[Int32], intervalSize: GUniform[ScanArgs]) extends Layout
      case class CompactLayout(cIn: GBuffer[C], intIn: GBuffer[Int32], out: GBuffer[C]) extends Layout
      case class FilterResult(cOut: GBuffer[C]) extends Layout

      val predicateProgram = GProgram[Params, PredLayout](
        layout = params => PredLayout(cIn = GBuffer[C](params.inSize), boolOut = GBuffer[Int32](params.inSize)),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize, 1, 1)),
      ): layout =>
        val invocId = GIO.invocationId
        val element = GIO.read[C](layout.cIn, invocId)
        GIO.write[Int32](layout.boolOut, invocId, when(pred(element))(1: Int32).otherwise(0))

      val predicateExec = GExecution[Params, PredLayout]()
        .addProgram(predicateProgram)(params => Params(256, 2), layout => PredLayout(layout.cIn, layout.boolOut))

      val upsweep = GProgram[Params, ScanLayout](
        layout = params =>
          ScanLayout(
            intIn = GBuffer[Int32](params.inSize / params.intervalSize),
            intOut = GBuffer[Int32](params.inSize),
            intervalSize = GUniform(ScanArgs(params.intervalSize)),
          ),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize / params.intervalSize, 1, 1)),
      ): layout =>
        val ScanArgs(size) = layout.intervalSize.read
        val invocId = GIO.invocationId
        val root = invocId * size
        val mid = root + (size / 2) - 1
        val end = root + size - 1
        val oldValue = GIO.read[Int32](layout.intOut, end)
        val addValue = GIO.read[Int32](layout.intOut, mid)
        val newValue = oldValue + addValue
        GIO.write[Int32](layout.intOut, end, newValue)

      val downsweep = GProgram[Params, ScanLayout](
        layout = params =>
          ScanLayout(
            intIn = GBuffer[Int32](params.inSize / params.intervalSize),
            intOut = GBuffer[Int32](params.inSize),
            intervalSize = GUniform(ScanArgs(params.intervalSize)),
          ),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize / params.intervalSize, 1, 1)),
      ): layout =>
        val ScanArgs(size) = layout.intervalSize.read
        val invocId = GIO.invocationId
        val root = invocId * size - 1 // if invocId = 0, this is -1 (out of bounds)
        val mid = root + (size / 2)
        val oldValue = GIO.read[Int32](layout.intOut, mid)
        val addValue = when(root > 0)(GIO.read[Int32](layout.intOut, root)).otherwise(0)
        val newValue = oldValue + addValue
        GIO.write[Int32](layout.intOut, mid, newValue)

      @annotation.tailrec
      def upsweepPhases(exec: GExecution[Params, ScanLayout, ?], inSize: Int, intervalSize: Int): GExecution[Params, ScanLayout, ?] =
        if intervalSize >= inSize then exec
        else
          val newExec = exec.addProgram(upsweep)(params => Params(inSize, intervalSize), layout => layout)
          upsweepPhases(newExec, inSize, intervalSize * 2)

      val upsweepInitialExec = GExecution[Params, ScanLayout]()
      val upsweepExec = upsweepPhases(upsweepInitialExec, 256, 2)

      @annotation.tailrec
      def downsweepPhases(exec: GExecution[Params, ScanLayout, ?], inSize: Int, intervalSize: Int): GExecution[Params, ScanLayout, ?] =
        if intervalSize < 2 then exec
        else
          val newExec = exec.addProgram(downsweep)(params => Params(inSize, intervalSize), layout => layout)
          downsweepPhases(newExec, inSize, intervalSize / 2)

      val downsweepExec = downsweepPhases(upsweepInitialExec, 256, 128)

      val startParams = Params(256, 2)
      // val region = GBufferRegion
      //   .allocate[ScanLayout]
      //   .map: region =>
      //     upsweepExec.execute(startParams, region)

      ???

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
