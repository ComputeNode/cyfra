package io.computenode.cyfra.core

import io.computenode.cyfra.core.GProgram.BufferSizeSpec
import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.buffer.GBuffer

import java.nio.ByteBuffer

sealed trait GBufferRegion[ReqAlloc <: Layout: LayoutStruct, ResAlloc <: Layout: LayoutStruct]:
  val initAlloc: ReqAlloc

object GBufferRegion:

  private[cyfra] case class ZeroedBuffer[T <: Value](size: Int) extends GBuffer[T]

  private[cyfra] case class BufferFromRam[T <: Value](buff: ByteBuffer) extends GBuffer[T]

  trait InitAlloc:
    extension (buffers: GBuffer.type)
      def apply[T <: Value](size: Int): GBuffer[T] =
        ZeroedBuffer[T](size)

      def apply[T <: Value](buff: ByteBuffer): GBuffer[T] =
        BufferFromRam[T](buff)

  trait FinalizeAlloc:
    extension [T <: Value](buffer: GBuffer[T])
      def readTo(bb: ByteBuffer): Unit =
        ()

  def allocate[Alloc <: Layout: LayoutStruct]: GBufferRegion[Alloc, Alloc] =
    AllocRegion(summon[LayoutStruct[Alloc]].layoutRef)

  case class AllocRegion[Alloc <: Layout: LayoutStruct](l: Alloc) extends GBufferRegion[Alloc, Alloc]:
    val initAlloc: Alloc = l

  case class MapRegion[ReqAlloc <: Layout: LayoutStruct, BodyAlloc <: Layout: LayoutStruct, ResAlloc <: Layout: LayoutStruct](
    reqRegion: GBufferRegion[ReqAlloc, BodyAlloc],
    f: Allocation => BodyAlloc => ResAlloc,
  ) extends GBufferRegion[ReqAlloc, ResAlloc]:
    val initAlloc: ReqAlloc = reqRegion.initAlloc

  extension [ReqAlloc <: Layout: LayoutStruct, ResAlloc <: Layout: LayoutStruct](region: GBufferRegion[ReqAlloc, ResAlloc])
    def map[NewAlloc <: Layout: LayoutStruct](f: Allocation ?=> ResAlloc => NewAlloc): GBufferRegion[ReqAlloc, NewAlloc] =
      MapRegion(region, (alloc: Allocation) => (resAlloc: ResAlloc) => f(using alloc)(resAlloc))

    def runUnsafe(init: InitAlloc ?=> ReqAlloc, onDone: FinalizeAlloc ?=> ResAlloc => Unit): Unit =
      val initAlloc = new InitAlloc {}
      init(using initAlloc)
      val alloc = new Allocation {}
      val steps: Seq[Allocation => Layout => Layout] = Seq.unfold(region: GBufferRegion[?, ?]):
        case _: AllocRegion[?] => None
        case MapRegion(req, f) =>
          Some((f.asInstanceOf[Allocation => Layout => Layout], req))

      val bodyAlloc = steps.foldLeft[Layout](region.initAlloc): (acc, step) =>
        step(alloc)(acc)

      val finalizeAlloc = new FinalizeAlloc {}
      onDone(using finalizeAlloc)(bodyAlloc.asInstanceOf[ResAlloc])
