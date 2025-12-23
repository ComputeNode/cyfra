package io.computenode.cyfra.dsl.archive.binding

import io.computenode.cyfra.dsl.archive.Value.Int32
import io.computenode.cyfra.dsl.archive.{Expression, Value}
import izumi.reflect.Tag

case class ReadBuffer[T <: Value: Tag](buffer: GBuffer[T], index: Int32) extends Expression[T]
