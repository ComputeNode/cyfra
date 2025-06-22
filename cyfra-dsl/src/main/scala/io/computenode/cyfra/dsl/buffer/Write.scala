package io.computenode.cyfra.dsl.buffer

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.gio.GIO

case class Write[T <: Value](buffer: GBuffer[T], index: Int32, value: T) extends GIO[Unit]:
  override def underlying: Unit = ()
