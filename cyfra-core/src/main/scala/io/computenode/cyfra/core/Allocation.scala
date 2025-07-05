package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.GBuffer
import izumi.reflect.Tag

import java.nio.ByteBuffer

trait Allocation:
  extension [R, T <: Value](buffer: GBuffer[T])
    def read(bb: ByteBuffer): Unit

    def write(bb: ByteBuffer): Unit

  extension [Params, L <: Layout, RL <: Layout: LayoutStruct](execution: GExecution[Params, L, RL]) def execute(params: Params, layout: L): RL

object Allocation:

  trait InitAlloc:
    extension (buffers: GBuffer.type)
      def apply[T <: Value: Tag: FromExpr](size: Int): GBuffer[T]

      def apply[T <: Value: Tag: FromExpr](buff: ByteBuffer): GBuffer[T]

  trait FinalizeAlloc:
    extension [T <: Value](buffer: GBuffer[T]) def readTo(bb: ByteBuffer): Unit
