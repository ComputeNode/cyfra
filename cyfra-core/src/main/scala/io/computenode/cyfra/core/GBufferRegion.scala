package io.computenode.cyfra.core

import io.computenode.cyfra.core.Allocation
import io.computenode.cyfra.core.GProgram.BufferLengthSpec
import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.GBuffer
import izumi.reflect.Tag

import java.nio.ByteBuffer

sealed trait GBufferRegion[ReqAlloc <: Layout: LayoutStruct, ResAlloc <: Layout: LayoutStruct]

object GBufferRegion:

  def allocate[Alloc <: Layout: LayoutStruct]: GBufferRegion[Alloc, Alloc] =
    AllocRegion(summon[LayoutStruct[Alloc]].layoutRef)

  case class AllocRegion[Alloc <: Layout: LayoutStruct](l: Alloc) extends GBufferRegion[Alloc, Alloc]

  case class MapRegion[ReqAlloc <: Layout: LayoutStruct, BodyAlloc <: Layout: LayoutStruct, ResAlloc <: Layout: LayoutStruct](
    reqRegion: GBufferRegion[ReqAlloc, BodyAlloc],
    f: Allocation => BodyAlloc => ResAlloc,
  ) extends GBufferRegion[ReqAlloc, ResAlloc]

  extension [ReqAlloc <: Layout: LayoutStruct, ResAlloc <: Layout: LayoutStruct](region: GBufferRegion[ReqAlloc, ResAlloc])
    def map[NewAlloc <: Layout: LayoutStruct](f: Allocation ?=> ResAlloc => NewAlloc): GBufferRegion[ReqAlloc, NewAlloc] =
      MapRegion(region, (alloc: Allocation) => (resAlloc: ResAlloc) => f(using alloc)(resAlloc))

    def runUnsafe(init: Allocation ?=> ReqAlloc, onDone: Allocation ?=> ResAlloc => Unit)(using cyfraRuntime: CyfraRuntime): Unit =
      cyfraRuntime.withAllocation: allocation =>

        // noinspection ScalaRedundantCast
        val steps: Seq[Allocation => Layout => Layout] = Seq.unfold(region: GBufferRegion[?, ?]):
          case _: AllocRegion[?] => None
          case MapRegion(req, f) =>
            Some((f.asInstanceOf[Allocation => Layout => Layout], req))

        val initAlloc = init(using allocation)
        val bodyAlloc = steps.foldLeft[Layout](initAlloc): (acc, step) =>
          step(allocation)(acc)

        onDone(using allocation)(bodyAlloc.asInstanceOf[ResAlloc])
