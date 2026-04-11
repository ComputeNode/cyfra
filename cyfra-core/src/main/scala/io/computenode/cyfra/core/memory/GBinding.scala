package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.types.GArray
import izumi.reflect.Tag

sealed trait GBinding[T: Value] extends FocusRoot[T]

trait GBuffer[T: Value] extends GBinding[T]

trait GUniform[T: Value] extends GBinding[T]

object GBuffer

object GBinding

object GUniform:
  class ParamUniform[T: Value] extends GUniform[T]
  def fromParams[T: Value]: ParamUniform[T] = ParamUniform[T]()
