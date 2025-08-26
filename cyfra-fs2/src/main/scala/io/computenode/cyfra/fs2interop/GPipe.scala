package io.computenode.cyfra.fs2interop

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
  def map[F[_], C1 <: Value: FromExpr: Tag, C2 <: Value: FromExpr: Tag, S1: ClassTag, S2: ClassTag](
    f: C1 => C2,
  )(using cr: CyfraRuntime, bridge1: Bridge[C1, S1], bridge2: Bridge[C2, S2]): Pipe[F, S1, S2] =
    (stream: Stream[F, S1]) =>
      case class Params(inSize: Int)
      case class PLayout(in: GBuffer[C1], out: GBuffer[C2]) extends Layout

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
          region.runUnsafe(init = PLayout(in = GBuffer[C1](inBuf), out = GBuffer[C2](outBuf)), onDone = layout => layout.out.read(outBuf))
          Stream.emits(bridge2.fromByteBuffer(outBuf, new Array[S2](params.inSize)))

  // Overload for convenient single type version
  def map[F[_], C <: Value: FromExpr: Tag, S: ClassTag](f: C => C)(using CyfraRuntime, Bridge[C, S]): Pipe[F, S, S] =
    map[F, C, C, S, S](f)

  def filter[F[_], C <: Value: FromExpr: Tag, S: ClassTag](pred: C => GBoolean)(using cr: CyfraRuntime, bridge: Bridge[C, S]): Pipe[F, S, S] =
    (stream: Stream[F, S]) =>
      case class Params(inSize: Int) // used commonly by many program layouts

      // Predicate mapping
      case class PredLayout(in: GBuffer[C], out: GBuffer[Int32]) extends Layout

      val predicateProgram = GProgram[Params, PredLayout](
        layout = params => PredLayout(in = GBuffer[C](params.inSize), out = GBuffer[Int32](params.inSize)),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize, 1, 1)),
      ): layout =>
        val invocId = GIO.invocationId
        val element = GIO.read[C](layout.in, invocId)
        val result = when(pred(element))(1: Int32).otherwise(0)
        GIO.write[Int32](layout.out, invocId, result)

      // Prefix sum (inclusive), upsweep/downsweep
      case class ScanParams(inSize: Int, intervalSize: Int)
      case class ScanArgs(intervalSize: Int32) extends GStruct[ScanArgs]
      case class ScanLayout(ints: GBuffer[Int32], intervalSize: GUniform[ScanArgs]) extends Layout

      val upsweep = GProgram[ScanParams, ScanLayout](
        layout = params => ScanLayout(ints = GBuffer[Int32](params.inSize), intervalSize = GUniform(ScanArgs(params.intervalSize))),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize / params.intervalSize, 1, 1)),
      ): layout =>
        val ScanArgs(size) = layout.intervalSize.read
        val invocId = GIO.invocationId
        val root = invocId * size
        val mid = root + (size / 2) - 1
        val end = root + size - 1
        val oldValue = GIO.read[Int32](layout.ints, end)
        val addValue = GIO.read[Int32](layout.ints, mid)
        val newValue = oldValue + addValue
        GIO.write[Int32](layout.ints, end, newValue)

      val downsweep = GProgram[ScanParams, ScanLayout](
        layout = params => ScanLayout(ints = GBuffer[Int32](params.inSize), intervalSize = GUniform(ScanArgs(params.intervalSize))),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize / params.intervalSize, 1, 1)),
      ): layout =>
        val ScanArgs(size) = layout.intervalSize.read
        val invocId = GIO.invocationId
        val end = invocId * size - 1 // if invocId = 0, this is -1 (out of bounds)
        val mid = end + (size / 2)
        val oldValue = GIO.read[Int32](layout.ints, mid)
        val addValue = when(end > 0)(GIO.read[Int32](layout.ints, end)).otherwise(0)
        val newValue = oldValue + addValue
        GIO.write[Int32](layout.ints, mid, newValue)

      // Stitch together many upsweep / downsweep program phases recursively
      @annotation.tailrec
      def upsweepPhases(
        exec: GExecution[ScanParams, ScanLayout, ScanLayout],
        inSize: Int,
        intervalSize: Int,
      ): GExecution[ScanParams, ScanLayout, ScanLayout] =
        if intervalSize >= inSize then exec
        else
          val newExec = exec.addProgram(upsweep)(params => ScanParams(inSize, intervalSize), layout => layout)
          upsweepPhases(newExec, inSize, intervalSize * 2)

      @annotation.tailrec
      def downsweepPhases(
        exec: GExecution[ScanParams, ScanLayout, ScanLayout],
        inSize: Int,
        intervalSize: Int,
      ): GExecution[ScanParams, ScanLayout, ScanLayout] =
        if intervalSize < 2 then exec
        else
          val newExec = exec.addProgram(downsweep)(params => ScanParams(inSize, intervalSize), layout => layout)
          downsweepPhases(newExec, inSize, intervalSize / 2)

      val initExec = GExecution[ScanParams, ScanLayout]() // no program
      val upsweepExec = upsweepPhases(initExec, 256, 2) // add all upsweep phases
      val scanExec = downsweepPhases(upsweepExec, 256, 128) // add all downsweep phases

      // Stream compaction
      case class CompactLayout(in: GBuffer[C], scan: GBuffer[Int32], out: GBuffer[C]) extends Layout

      val compactProgram = GProgram[Params, CompactLayout](
        layout = params => CompactLayout(in = GBuffer[C](params.inSize), scan = GBuffer[Int32](params.inSize), out = GBuffer[C](params.inSize)),
        dispatch = (layout, params) => GProgram.StaticDispatch((params.inSize, 1, 1)),
      ): layout =>
        val invocId = GIO.invocationId
        val element = GIO.read[C](layout.in, invocId)
        val prefixSum = GIO.read[Int32](layout.scan, invocId)
        val prevScan = when(invocId > 0)(GIO.read[Int32](layout.scan, invocId - 1)).otherwise(prefixSum)
        val condt = when(invocId > 0)(when(prevScan < prefixSum)(1: Int32).otherwise(0)).otherwise(when(prefixSum > 0)(1: Int32).otherwise(0))
        val index = when(invocId > 0)(when(prevScan < prefixSum)(prevScan).otherwise(0)).otherwise(0)
        GIO.repeat(condt): _ =>
          GIO.write[C](layout.out, index, element)

      // connect all the layouts/executions into one
      case class FilterLayout(in: GBuffer[C], scan: GBuffer[Int32], out: GBuffer[C]) extends Layout

      val filterExec = GExecution[Params, FilterLayout]()
      // val filterExec = predicateProgram
      //   .flatMap[ScanLayout, ScanParams]: predLayout =>
      //     scanExec
      //       .contramap[ScanLayout](layout => ScanLayout(???, ???))
      //       .contramapParams[ScanParams](params => ScanParams(???, ???))
      //       .map(_ => predLayout)
      //   .flatMap[FilterLayout, Params]: scanLayout =>
      //     compactProgram
      //       .contramap[FilterLayout](layout => CompactLayout(???, ???, ???))
      //       .contramapParams[Params](params => params)
      //       .map(_ => scanLayout)

      val filterParams = Params(256) // TODO

      // case class FilterParams(intervalSize: ScanArgs())
      // case class FilterLayout(inBuf: GBuffer[C], scanBuf: GBuffer[Int32])
      // val filterExec = GExecution[FilterParams, FilterLayout]()
      //   .addProgram(predicateProgram)(params => PredParams(params.inSize), layout => PredLayout(layout.in, layout.pred))
      //   .flatMap[FilterLayout, FilterParams](l =>
      //     downsweepExec
      //       .contramap[FilterLayout](layout => ScanLayout(layout.pred, layout.intervalSize))
      //       .contramapParams[FilterParams](p => ScanParams(p.inSize, 2))
      //       .map(_ => l)
      //   )

      val region = GBufferRegion
        .allocate[FilterLayout]
        .map: layout =>
          filterExec.execute(filterParams, layout)

      val typeSize = typeStride(Tag.apply[C])
      val intSize = typeStride(Tag.apply[Int32])
      val params = Params(256)

      // these are allocated once, reused for many chunks
      val predBuf = BufferUtils.createByteBuffer(params.inSize * typeSize)
      val scanBuf = BufferUtils.createByteBuffer(params.inSize * intSize)
      val compactBuf = BufferUtils.createByteBuffer(256 * typeSize)

      stream
        .chunkMin(filterParams.inSize)
        .flatMap: chunk =>
          bridge.toByteBuffer(predBuf, chunk)
          region.runUnsafe(
            init = FilterLayout(in = GBuffer[C](predBuf), scan = GBuffer[Int32](scanBuf), out = GBuffer[C](compactBuf)),
            onDone = layout => layout.out.read(compactBuf),
          )
          val arr = bridge.fromByteBuffer(compactBuf, new Array[S](256))
          Stream.emits(arr)
