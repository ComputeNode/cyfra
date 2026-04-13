package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Float32 extends FloatType
object Float32:
  def apply(value: Float): Float32 = const(value)
  given Value.Scalar[Float32] with
    protected def extractUnsafe(ir: ExpressionBlock[Float32]): Float32 = new Float32Impl(ir)
    def tag: Tag[Float32] = Tag[Float32]
