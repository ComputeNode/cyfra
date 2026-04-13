package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class UInt32 extends UnsignedIntType
object UInt32:
  def apply(value: Int): UInt32 = const(value)
  given Value.Scalar[UInt32] with
    protected def extractUnsafe(ir: ExpressionBlock[UInt32]): UInt32 = new UInt32Impl(ir)
    def tag: Tag[UInt32] = Tag[UInt32]
