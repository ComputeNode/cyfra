package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Int16 extends SignedIntType
object Int16:
  def apply(value: Int): Int16 = const(value)
  given Value.Scalar[Int16] with
    protected def extractUnsafe(ir: ExpressionBlock[Int16]): Int16 = new Int16Impl(ir)
    def tag: Tag[Int16] = Tag[Int16]
