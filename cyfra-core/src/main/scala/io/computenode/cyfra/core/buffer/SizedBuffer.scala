package io.computenode.cyfra.core.buffer

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.buffer.GBuffer

case class SizedBuffer[T <: Value](size: Int) extends GBuffer[T]
