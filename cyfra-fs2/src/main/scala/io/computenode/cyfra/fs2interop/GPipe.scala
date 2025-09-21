package io.computenode.cyfra.fs2interop

import io.computenode.cyfra.core.{Allocation, layout, GCodec}
import layout.Layout
import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.layout.LayoutBinding
import io.computenode.cyfra.core.layout.LayoutStruct
import gio.GIO
import binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import struct.GStruct
import GStruct.Empty
import Empty.given
import fs2.*

import java.nio.ByteBuffer
import org.lwjgl.BufferUtils
import izumi.reflect.Tag

import scala.reflect.ClassTag

object GPipe:
  def map[F[_], C1 <: Value: {FromExpr, Tag}, C2 <: Value: {FromExpr, Tag}, S1: ClassTag, S2: ClassTag](
    f: C1 => C2,
  )(using cr: CyfraRuntime, bridge1: GCodec[C1, S1], bridge2: GCodec[C2, S2]): Pipe[F, S1, S2] =
    (stream: Stream[F, S1]) =>
      case class Params(inSize: Int)
      case class PLayout(in: GBuffer[C1], out: GBuffer[C2]) extends Layout

      val params = Params(inSize = 256)
      val inTypeSize = typeStride(Tag.apply[C1])
      val outTypeSize = typeStride(Tag.apply[C2])

      val gProg = GProgram[Params, PLayout](
        layout = params => PLayout(in = GBuffer[C1](params.inSize), out = GBuffer[C2](params.inSize)),
        dispatch = (layout, params) => GProgram.StaticDispatch((Math.ceil(params.inSize / 256f).toInt, 1, 1)),
      ) { layout =>
        val invocId = GIO.invocationId
        val element = GIO.read[C1](layout.in, invocId)
        val res = f(element)
        for _ <- GIO.write[C2](layout.out, invocId, res)
        yield Empty()
      }

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
        .chunkN(params.inSize)
        .flatMap: chunk =>
          bridge1.toByteBuffer(inBuf, chunk.toArray)
          region.runUnsafe(init = PLayout(in = GBuffer[C1](inBuf), out = GBuffer[C2](outBuf)), onDone = layout => layout.out.read(outBuf))
          Stream.emits(bridge2.fromByteBuffer(outBuf, new Array[S2](params.inSize)))

  // Overload for convenient single type version
  def map[F[_], C <: Value: FromExpr: Tag, S: ClassTag](f: C => C)(using CyfraRuntime, GCodec[C, S]): Pipe[F, S, S] =
    map[F, C, C, S, S](f)

  def filter[F[_], C <: Value: FromExpr: Tag, S: ClassTag](pred: C => GBoolean)(using cr: CyfraRuntime, bridge: GCodec[C, S]): Pipe[F, S, S] =
    (stream: Stream[F, S]) =>
      val chunkInSize = 256

      // Predicate mapping
      case class PredParams(inSize: Int)
      case class PredLayout(in: GBuffer[C], out: GBuffer[Int32]) extends Layout

      val predicateProgram = GProgram[PredParams, PredLayout](
        layout = params => PredLayout(in = GBuffer[C](params.inSize), out = GBuffer[Int32](params.inSize)),
        dispatch = (layout, params) => GProgram.StaticDispatch((Math.ceil(params.inSize.toFloat / 256).toInt, 1, 1)),
      ): layout =>
        val invocId = GIO.invocationId
        val element = GIO.read[C](layout.in, invocId)
        val result = when(pred(element))(1: Int32).otherwise(0)
        for _ <- GIO.write[Int32](layout.out, invocId, result)
        yield Empty()

      // Prefix sum (inclusive), upsweep/downsweep
      case class ScanParams(inSize: Int, intervalSize: Int)
      case class ScanArgs(intervalSize: Int32) extends GStruct[ScanArgs]
      case class ScanLayout(ints: GBuffer[Int32]) extends Layout
      case class ScanProgramLayout(ints: GBuffer[Int32], intervalSize: GUniform[ScanArgs] = GUniform.fromParams) extends Layout

      val upsweep = GProgram[ScanParams, ScanProgramLayout](
        layout = params => ScanProgramLayout(ints = GBuffer[Int32](params.inSize), intervalSize = GUniform(ScanArgs(params.intervalSize))),
        dispatch = (layout, params) => GProgram.StaticDispatch((Math.ceil(params.inSize.toFloat / params.intervalSize / 256).toInt, 1, 1)),
      ): layout =>
        val ScanArgs(size) = layout.intervalSize.read
        GIO.when(GIO.invocationId < ((chunkInSize: Int32) / size)):
          val invocId = GIO.invocationId
          val root = invocId * size
          val mid = root + (size / 2) - 1
          val end = root + size - 1
          val oldValue = GIO.read[Int32](layout.ints, end)
          val addValue = GIO.read[Int32](layout.ints, mid)
          val newValue = oldValue + addValue
          for _ <- GIO.write[Int32](layout.ints, end, newValue)
          yield Empty()

      val downsweep = GProgram[ScanParams, ScanProgramLayout](
        layout = params => ScanProgramLayout(ints = GBuffer[Int32](params.inSize), intervalSize = GUniform(ScanArgs(params.intervalSize))),
        dispatch = (layout, params) => GProgram.StaticDispatch((Math.ceil(params.inSize.toFloat / params.intervalSize / 256).toInt, 1, 1)),
      ): layout =>
        val ScanArgs(size) = layout.intervalSize.read
        GIO.when(GIO.invocationId < ((chunkInSize: Int32) / size)):
          val invocId = GIO.invocationId
          val end = invocId * size - 1 // if invocId = 0, this is -1 (out of bounds)
          val mid = end + (size / 2)
          val oldValue = GIO.read[Int32](layout.ints, mid)
          val addValue = when(end > 0)(GIO.read[Int32](layout.ints, end)).otherwise(0)
          val newValue = oldValue + addValue
          for _ <- GIO.write[Int32](layout.ints, mid, newValue)
          yield Empty()

      // Stitch together many upsweep / downsweep program phases recursively
      @annotation.tailrec
      def upsweepPhases(
        exec: GExecution[ScanParams, ScanLayout, ScanLayout],
        inSize: Int,
        intervalSize: Int,
      ): GExecution[ScanParams, ScanLayout, ScanLayout] =
        if intervalSize > inSize then exec
        else
          val newExec = exec.addProgram(upsweep)(params => ScanParams(inSize, intervalSize), layout => ScanProgramLayout(layout.ints))
          upsweepPhases(newExec, inSize, intervalSize * 2)

      @annotation.tailrec
      def downsweepPhases(
        exec: GExecution[ScanParams, ScanLayout, ScanLayout],
        inSize: Int,
        intervalSize: Int,
      ): GExecution[ScanParams, ScanLayout, ScanLayout] =
        if intervalSize < 2 then exec
        else
          val newExec = exec.addProgram(downsweep)(params => ScanParams(inSize, intervalSize), layout => ScanProgramLayout(layout.ints))
          downsweepPhases(newExec, inSize, intervalSize / 2)

      val initExec = GExecution[ScanParams, ScanLayout]() // no program
      val upsweepExec = upsweepPhases(initExec, 256, 2) // add all upsweep phases
      val scanExec = downsweepPhases(upsweepExec, 256, 128) // add all downsweep phases

      // Stream compaction
      case class CompactParams(inSize: Int)
      case class CompactLayout(in: GBuffer[C], scan: GBuffer[Int32], out: GBuffer[C]) extends Layout

      val compactProgram = GProgram[CompactParams, CompactLayout](
        layout = params => CompactLayout(in = GBuffer[C](params.inSize), scan = GBuffer[Int32](params.inSize), out = GBuffer[C](params.inSize)),
        dispatch = (layout, params) => GProgram.StaticDispatch((Math.ceil(params.inSize.toFloat / 256).toInt, 1, 1)),
      ): layout =>
        val invocId = GIO.invocationId
        val element = GIO.read[C](layout.in, invocId)
        val prefixSum = GIO.read[Int32](layout.scan, invocId)
        for
          _ <- GIO.when(invocId > 0):
            val prevScan = GIO.read[Int32](layout.scan, invocId - 1)
            GIO.when(prevScan < prefixSum):
              GIO.write(layout.out, prevScan, element)
          _ <- GIO.when(invocId === 0):
            GIO.when(prefixSum > 0):
              GIO.write(layout.out, invocId, element)
        yield Empty()

      // connect all the layouts/executions into one
      case class FilterParams(inSize: Int, intervalSize: Int)
      case class FilterLayout(in: GBuffer[C], scan: GBuffer[Int32], out: GBuffer[C]) extends Layout

      val filterExec = GExecution[FilterParams, FilterLayout]()
        .addProgram(predicateProgram)(
          filterParams => PredParams(filterParams.inSize),
          filterLayout => PredLayout(in = filterLayout.in, out = filterLayout.scan),
        )
        .flatMap[FilterLayout, FilterParams]: filterLayout =>
          scanExec
            .contramap[FilterLayout]: filterLayout =>
              ScanLayout(filterLayout.scan)
            .contramapParams[FilterParams](filterParams => ScanParams(filterParams.inSize, filterParams.intervalSize))
            .map(scanLayout => filterLayout)
        .flatMap[FilterLayout, FilterParams]: filterLayout =>
          compactProgram
            .contramap[FilterLayout]: filterLayout =>
              CompactLayout(filterLayout.in, filterLayout.scan, filterLayout.out)
            .contramapParams[FilterParams](filterParams => CompactParams(filterParams.inSize))
            .map(compactLayout => filterLayout)

      // finally setup buffers, region, parameters, and run the program
      val filterParams = FilterParams(chunkInSize, 2)
      val region = GBufferRegion
        .allocate[FilterLayout]
        .map: filterLayout =>
          filterExec.execute(filterParams, filterLayout)

      val typeSize = typeStride(Tag.apply[C])
      val intSize = typeStride(Tag.apply[Int32])

      // these are allocated once, reused for many chunks
      val predBuf = BufferUtils.createByteBuffer(filterParams.inSize * typeSize)
      val filteredCount = BufferUtils.createByteBuffer(intSize)
      val compactBuf = BufferUtils.createByteBuffer(filterParams.inSize * typeSize)

      stream
        .chunkN(chunkInSize)
        .flatMap: chunk =>
          bridge.toByteBuffer(predBuf, chunk.toArray)
          region.runUnsafe(
            init = FilterLayout(in = GBuffer[C](predBuf), scan = GBuffer[Int32](filterParams.inSize), out = GBuffer[C](filterParams.inSize)),
            onDone = layout => {
              layout.scan.read(filteredCount, (filterParams.inSize - 1) * intSize)
              layout.out.read(compactBuf)
            },
          )
          val filteredN = filteredCount.getInt(0)
          val arr = bridge.fromByteBuffer(compactBuf, new Array[S](filteredN))
          println(arr)
          Stream.emits(arr)
