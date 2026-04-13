package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class UInt16 extends UnsignedIntType
object UInt16:
  def apply(value: Int): UInt16 = const(value)
  given Value.Scalar[UInt16] with
    protected def extractUnsafe(ir: ExpressionBlock[UInt16]): UInt16 = new UInt16Impl(ir)
    def tag: Tag[UInt16] = Tag[UInt16]
