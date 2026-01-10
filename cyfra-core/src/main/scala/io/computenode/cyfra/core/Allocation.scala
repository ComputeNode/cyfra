package io.computenode.cyfra.core

import io.computenode.cyfra.core.layout.{Layout, LayoutBinding}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import izumi.reflect.Tag

import java.nio.ByteBuffer
import scala.reflect.ClassTag

trait Allocation:
  def submitLayout[L <: Layout: LayoutBinding](layout: L): Unit

  extension (buffer: GBinding[?])
    def read(bb: ByteBuffer, offset: Int = 0): Unit

    def write(bb: ByteBuffer, offset: Int = 0)(using name: sourcecode.FileName, line: sourcecode.Line): Unit


  extension [T <: Value: {Tag, FromExpr}] (buffer: GBinding[T])

    def readArray[ST : ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Array[ST]

    def writeArray[ST : ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Unit

  extension [Params, EL <: Layout: LayoutBinding, RL <: Layout: LayoutBinding](execution: GExecution[Params, EL, RL])
    def execute(params: Params, layout: EL)(using name: sourcecode.FileName, line: sourcecode.Line): RL

  extension (buffers: GBuffer.type)
    def apply[T <: Value: {Tag, FromExpr}](length: Int): GBuffer[T]

    def apply[ST : ClassTag, T <: Value: {Tag, FromExpr}](scalaArray: Array[ST])(using GCodec[T, ST]): GBuffer[T]

    def apply[T <: Value: {Tag, FromExpr}](buff: ByteBuffer)(using name: sourcecode.FileName, line: sourcecode.Line): GBuffer[T]

  extension (buffers: GUniform.type)
    def apply[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](buff: ByteBuffer)(using name: sourcecode.FileName, line: sourcecode.Line): GUniform[T]

    def apply[ST : ClassTag, T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](value: ST)(using GCodec[T, ST]): GUniform[T]

    def apply[T <: GStruct[T]: {Tag, FromExpr, GStructSchema}](): GUniform[T]
