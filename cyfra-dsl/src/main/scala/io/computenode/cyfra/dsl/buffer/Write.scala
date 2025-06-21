package io.computenode.cyfra.dsl.buffer

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.gio.GIO

case class Write[T <: Value](buffer: GBuffer[T], index: Int, value: T) extends GIO[Unit]:
  override def underlying: Unit = ()
