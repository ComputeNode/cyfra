package io.computenode.cyfra.dsl.buffer

import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

case class Read[T <: Value: Tag](buffer: GBuffer[T], index: Int32) extends Expression[T]
