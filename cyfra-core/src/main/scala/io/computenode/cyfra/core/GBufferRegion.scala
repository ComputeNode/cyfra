package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}

sealed trait GBufferRegion[ReqAlloc <: Layout: LayoutStruct, ResAlloc <: Layout: LayoutStruct]:
  val initAlloc: ReqAlloc

object GBufferRegion:

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

    def runUnsafe(init: Allocation ?=> ReqAlloc, onDone: Allocation ?=> ResAlloc => Unit): Unit =
      val alloc = new Allocation {}
      init(using alloc)
      val steps: Seq[Allocation => Layout => Layout] = Seq.unfold(region: GBufferRegion[?, ?]):
        case _: AllocRegion[?] => None
        case MapRegion(req, f) =>
          Some((f.asInstanceOf[Allocation => Layout => Layout], req))

      val bodyAlloc = steps.foldLeft[Layout](region.initAlloc): (acc, step) =>
        step(alloc)(acc)

      onDone(using alloc)(bodyAlloc.asInstanceOf[ResAlloc])
