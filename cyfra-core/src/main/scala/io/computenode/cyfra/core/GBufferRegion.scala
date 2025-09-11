package io.computenode.cyfra.core

import io.computenode.cyfra.core.Allocation
import io.computenode.cyfra.core.GBufferRegion.MapRegion
import io.computenode.cyfra.core.GProgram.BufferLengthSpec
import io.computenode.cyfra.core.layout.{Layout, LayoutBinding}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.GBuffer
import izumi.reflect.Tag

import scala.util.chaining.given
import java.nio.ByteBuffer

sealed trait GBufferRegion[ReqAlloc <: Layout: LayoutBinding, ResAlloc <: Layout: LayoutBinding]:
  def reqAllocBinding: LayoutBinding[ReqAlloc] = summon[LayoutBinding[ReqAlloc]]
  def resAllocBinding: LayoutBinding[ResAlloc] = summon[LayoutBinding[ResAlloc]]

  def map[NewAlloc <: Layout: LayoutBinding](f: Allocation ?=> ResAlloc => NewAlloc): GBufferRegion[ReqAlloc, NewAlloc] =
    MapRegion(this, (alloc: Allocation) => (resAlloc: ResAlloc) => f(using alloc)(resAlloc))

object GBufferRegion:

  def allocate[Alloc <: Layout: LayoutBinding]: GBufferRegion[Alloc, Alloc] = AllocRegion()

  case class AllocRegion[Alloc <: Layout: LayoutBinding]() extends GBufferRegion[Alloc, Alloc]

  case class MapRegion[ReqAlloc <: Layout: LayoutBinding, BodyAlloc <: Layout: LayoutBinding, ResAlloc <: Layout: LayoutBinding](
    reqRegion: GBufferRegion[ReqAlloc, BodyAlloc],
    f: Allocation => BodyAlloc => ResAlloc,
  ) extends GBufferRegion[ReqAlloc, ResAlloc]

  extension [ReqAlloc <: Layout: LayoutBinding, ResAlloc <: Layout: LayoutBinding](region: GBufferRegion[ReqAlloc, ResAlloc])
    def runUnsafe(init: Allocation ?=> ReqAlloc, onDone: Allocation ?=> ResAlloc => Unit)(using cyfraRuntime: CyfraRuntime): Unit =
      cyfraRuntime.withAllocation: allocation =>

        // noinspection ScalaRedundantCast
        val steps: Seq[(Allocation => Layout => Layout, LayoutBinding[Layout])] = Seq.unfold(region: GBufferRegion[?, ?]):
          case AllocRegion       => None
          case MapRegion(req, f) =>
            Some(((f.asInstanceOf[Allocation => Layout => Layout], req.resAllocBinding.asInstanceOf[LayoutBinding[Layout]]), req))

        val initAlloc = init(using allocation).tap(allocation.submitLayout)
        val bodyAlloc = steps.foldLeft[Layout](initAlloc): (acc, step) =>
          step._1(allocation)(acc).tap(allocation.submitLayout(_)(using step._2))

        onDone(using allocation)(bodyAlloc.asInstanceOf[ResAlloc])
