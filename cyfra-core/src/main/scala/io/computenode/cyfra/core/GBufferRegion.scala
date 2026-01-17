package io.computenode.cyfra.core

import io.computenode.cyfra.core.Allocation
import io.computenode.cyfra.core.GBufferRegion.MapRegion
import io.computenode.cyfra.core.GProgram.BufferLengthSpec
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.GBuffer
import izumi.reflect.Tag

import scala.util.chaining.given
import java.nio.ByteBuffer

sealed trait GBufferRegion[ReqAlloc: Layout, ResAlloc: Layout]:
  def reqAllocLayout: Layout[ReqAlloc] = Layout[ReqAlloc]
  def resAllocLayout: Layout[ResAlloc] = Layout[ResAlloc]

  def map[NewAlloc: Layout](f: Allocation ?=> ResAlloc => NewAlloc): GBufferRegion[ReqAlloc, NewAlloc] =
    MapRegion(this, (alloc: Allocation) => (resAlloc: ResAlloc) => f(using alloc)(resAlloc))

object GBufferRegion:

  def allocate[Alloc: Layout]: GBufferRegion[Alloc, Alloc] = AllocRegion()

  case class AllocRegion[Alloc: Layout]() extends GBufferRegion[Alloc, Alloc]

  case class MapRegion[ReqAlloc: Layout, BodyAlloc: Layout, ResAlloc: Layout](
    reqRegion: GBufferRegion[ReqAlloc, BodyAlloc],
    f: Allocation => BodyAlloc => ResAlloc,
  ) extends GBufferRegion[ReqAlloc, ResAlloc]

  extension [ReqAlloc: Layout, ResAlloc: Layout](region: GBufferRegion[ReqAlloc, ResAlloc])
    def runUnsafe(init: Allocation ?=> ReqAlloc, onDone: Allocation ?=> ResAlloc => Unit)(using cyfraRuntime: CyfraRuntime): Unit =
      cyfraRuntime.withAllocation: allocation =>

        // noinspection ScalaRedundantCast
        val steps: Seq[(Allocation => Any => Any, Layout[Any])] = Seq.unfold(region: GBufferRegion[?, ?]):
          case AllocRegion()     => None
          case MapRegion(req, f) =>
            Some(((f.asInstanceOf[Allocation => Any => Any], req.resAllocLayout.asInstanceOf[Layout[Any]]), req))

        val initAlloc = init(using allocation).tap(allocation.submitLayout)

        val bodyAlloc = steps.reverse.foldLeft[Any](initAlloc): (acc, step) =>
          step._1(allocation)(acc).tap(allocation.submitLayout(_)(using step._2))

        onDone(using allocation)(bodyAlloc.asInstanceOf[ResAlloc])
