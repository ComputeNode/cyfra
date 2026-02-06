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
  // Cache the underlying value to ensure stable treeid for compiler lookups
  private lazy val _underlying: Empty = Empty()
  override def underlying: Empty = _underlying
