package io.computenode.cyfra.core.binding

import izumi.reflect.Tag
import io.computenode.cyfra.core.expression.Value

case class BufferRef[T: Value](layoutOffset: Int, valueTag: Tag[T]) extends GBuffer[T]
