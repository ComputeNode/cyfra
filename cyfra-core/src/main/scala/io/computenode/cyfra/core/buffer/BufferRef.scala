package io.computenode.cyfra.core.buffer

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.buffer.GBuffer
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

case class BufferRef[T <: Value](layoutOffset: Int, valueTag: Tag[T]) extends GBuffer[T]
