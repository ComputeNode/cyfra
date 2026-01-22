package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Expression
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import izumi.reflect.Tag

case class ReadShared[T <: Value: {Tag, FromExpr}](
  buffer: GShared[T],
  index: Int32,
) extends Expression[T]
