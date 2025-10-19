package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty

case class WriteBuffer[T <: Value](buffer: GBuffer[T], index: Int32, value: T) extends GIO[Empty]:
  override def underlying: Empty = Empty()
