package io.computenode.cyfra.dsl.buffer

import io.computenode.cyfra.dsl.{Expression, Value}

case class Read[T <: Value](buffer: GBuffer[T], index: Int) extends Expression[T]