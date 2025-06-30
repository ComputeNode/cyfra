package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

case class ReadBuffer[T <: Value: Tag](buffer: GBuffer[T], index: Int32) extends Expression[T]
