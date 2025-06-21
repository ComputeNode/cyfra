package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.LayoutStruct
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.core.layout.Layout

import java.nio.ByteBuffer
import GProgram.*
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.buffer.GBuffer
import izumi.reflect.Tag

case class GProgram[Params, L <: Layout: LayoutStruct] private (body: L => GIO[?], layout: BufferOfSize => Params => L, workgroupSize: WorkgroupSize):
  private[cyfra] def layoutStruct: LayoutStruct[L] = summon[LayoutStruct[L]]

object GProgram:
  type WorkgroupSize = (Int, Int, Int)

  private[cyfra] case class BufferSizeSpec[T <: Value](size: Int) extends GBuffer[T]
  
  trait BufferOfSize:
    extension (buffers: GBuffer.type)
      def apply[T <: Value](size: Int): BufferSizeSpec[T] =
        BufferSizeSpec[T](size)


  def apply[Params, L <: Layout : LayoutStruct](layout: BufferOfSize ?=> Params => L, workgroupSize: WorkgroupSize = (128, 1, 1))(body: L => GIO[?]): GProgram[Params, L] =
    new GProgram(body, s => layout(using s), workgroupSize)
