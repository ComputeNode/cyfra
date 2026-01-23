package io.computenode.cyfra.core.binding

import io.computenode.cyfra.core.expression.Value

sealed trait GBinding[T: Value]:
  def v: Value[T] = Value[T]

object GBinding

trait GBuffer[T: Value] extends GBinding[T]

object GBuffer

trait GUniform[T: Value] extends GBinding[T]

object GUniform:
  class ParamUniform[T: Value] extends GUniform[T]
  def fromParams[T: Value]: ParamUniform[T] = ParamUniform[T]()
