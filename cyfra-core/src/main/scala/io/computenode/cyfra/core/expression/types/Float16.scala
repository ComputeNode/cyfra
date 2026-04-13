package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Float16 extends FloatType
object Float16:
  def apply(value: Float): Float16 = const(value)
  given Value.Scalar[Float16] with
    protected def extractUnsafe(ir: ExpressionBlock[Float16]): Float16 = new Float16Impl(ir)
    def tag: Tag[Float16] = Tag[Float16]
  final class Float16Impl(val block: ExpressionBlock[Float16]) extends Float16 with ExpressionHolder[Float16]
