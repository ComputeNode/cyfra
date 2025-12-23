package io.computenode.cyfra.core.binding

import izumi.reflect.Tag
import io.computenode.cyfra.core.expression.Value

case class UniformRef[T: Value](layoutOffset: Int, valueTag: Tag[T]) extends GUniform[T]
