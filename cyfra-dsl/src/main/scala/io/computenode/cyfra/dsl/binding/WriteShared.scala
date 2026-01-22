package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

case class WriteShared[T <: Value: {Tag, FromExpr}](
  buffer: GShared[T],
  index: Int32,
  value: T,
) extends GIO[Empty]:
  override def underlying: Empty = Empty()
