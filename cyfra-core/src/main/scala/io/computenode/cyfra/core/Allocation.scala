package io.computenode.cyfra.core

import io.computenode.cyfra.core.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.layout.Layout
import izumi.reflect.Tag

import java.nio.ByteBuffer
import scala.reflect.ClassTag

trait Allocation:
  def submitLayout[L: Layout](layout: L): Unit

  extension (buffer: GBinding[?])
    def read(bb: ByteBuffer, offset: Int = 0): Unit

    def write(bb: ByteBuffer, offset: Int = 0): Unit

  extension [T: Value](buffer: GBinding[T])

    def readArray[ST: ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Array[ST]

    def writeArray[ST: ClassTag](arr: Array[ST], offset: Int = 0)(using GCodec[T, ST]): Unit

  extension [Params, EL: Layout, RL: Layout](execution: GExecution[Params, EL, RL]) def execute(params: Params, layout: EL): RL

  extension (buffers: GBuffer.type)
    def apply[T: Value](length: Int): GBuffer[T]

    def apply[ST: ClassTag, T: Value](scalaArray: Array[ST])(using GCodec[T, ST]): GBuffer[T]

    def apply[T: Value](buff: ByteBuffer): GBuffer[T]

  extension (buffers: GUniform.type)
    def apply[T: Value](buff: ByteBuffer): GUniform[T]

    def apply[ST: ClassTag, T: Value](value: ST)(using GCodec[T, ST]): GUniform[T]

    def apply[T: Value](): GUniform[T]
