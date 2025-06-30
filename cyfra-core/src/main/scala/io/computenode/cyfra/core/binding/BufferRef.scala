package io.computenode.cyfra.core.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.binding.GBuffer
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

case class BufferRef[T <: Value](layoutOffset: Int, valueTag: Tag[T]) extends GBuffer[T]
