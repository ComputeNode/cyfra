package io.computenode.cyfra.core.binding

import io.computenode.cyfra.core.expression.Value
import izumi.reflect.Tag

sealed trait BindingRef[T: Value]:
  val layoutOffset: Int

case class BufferRef[T: Value](layoutOffset: Int) extends BindingRef[T] with GBuffer[T]
case class UniformRef[T: Value](layoutOffset: Int) extends BindingRef[T] with GUniform[T]
