package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.GStruct
import izumi.reflect.Tag

import java.nio.ByteBuffer

trait Allocation:
  extension (buffer: GBinding[?])
    def read(bb: ByteBuffer, offset: Int = 0, length: Int = -1): Unit

    def write(bb: ByteBuffer, offset: Int = 0, length: Int = -1): Unit

  extension [Params, L <: Layout, RL <: Layout: LayoutStruct](execution: GExecution[Params, L, RL]) def execute(params: Params, layout: L): RL

  extension (buffers: GBuffer.type)
    def apply[T <: Value: Tag: FromExpr](size: Int): GBuffer[T]

    def apply[T <: Value: Tag: FromExpr](buff: ByteBuffer): GBuffer[T]

  extension (buffers: GUniform.type)
    def apply[T <: Value : Tag : FromExpr](buff: ByteBuffer): GUniform[T]

    def apply[T <: Value : Tag : FromExpr](): GUniform[T]


