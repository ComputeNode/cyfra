package io.computenode.cyfra.dsl.archive.binding

import io.computenode.cyfra.dsl.archive.Value.Int32
import io.computenode.cyfra.dsl.archive.Value
import io.computenode.cyfra.dsl.archive.gio.GIO
import io.computenode.cyfra.dsl.archive.struct.GStruct.Empty

case class WriteBuffer[T <: Value](buffer: GBuffer[T], index: Int32, value: T) extends GIO[Empty]:
  override def underlying: Empty = Empty()
