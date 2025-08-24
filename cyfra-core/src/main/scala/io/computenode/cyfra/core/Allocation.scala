package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.{Layout, LayoutBinding}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import izumi.reflect.Tag

import java.nio.ByteBuffer

trait Allocation:
  extension (buffer: GBinding[?])
    def read(bb: ByteBuffer, offset: Int = 0): Unit

    def write(bb: ByteBuffer, offset: Int = 0): Unit

  extension [Params, EL <: Layout: LayoutBinding, RL <: Layout: LayoutBinding](execution: GExecution[Params, EL, RL])
    def execute(params: Params, layout: EL): RL

  extension (buffers: GBuffer.type)
    def apply[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T]

    def apply[T <: Value: {Tag, FromExpr}](buff: ByteBuffer): GBuffer[T]

  extension (buffers: GUniform.type)
    def apply[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](buff: ByteBuffer): GUniform[T]

    def apply[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](): GUniform[T]
