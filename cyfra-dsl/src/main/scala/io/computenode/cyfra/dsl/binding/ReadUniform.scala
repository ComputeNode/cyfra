package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

case class ReadUniform[T <: Value: Tag](uniform: GUniform[T]) extends Expression[T]
