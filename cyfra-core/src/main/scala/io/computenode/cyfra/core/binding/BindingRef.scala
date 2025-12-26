package io.computenode.cyfra.core.binding

import io.computenode.cyfra.core.expression.Value
import izumi.reflect.Tag

sealed trait BindingRef[T: Value]:
  val layoutOffset: Int
  val valueTag: Tag[T]

case class BufferRef[T: Value](layoutOffset: Int, valueTag: Tag[T]) extends BindingRef[T] with GBuffer[T]
case class UniformRef[T: Value](layoutOffset: Int, valueTag: Tag[T]) extends BindingRef[T] with GUniform[T]
