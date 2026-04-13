package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.IntegerOps
import izumi.reflect.Tag

abstract class UInt16 extends UnsignedIntType with IntegerOps[UInt16]
object UInt16:
  final class UInt16Impl(val block: ExpressionBlock[UInt16]) extends UInt16 with ExpressionHolder[UInt16]

  given Value.Scalar[UInt16] with
    protected def extractUnsafe(ir: ExpressionBlock[UInt16]): UInt16 = new UInt16Impl(ir)
    def tag: Tag[UInt16] = Tag[UInt16]

  def apply(value: Int): UInt16 = const(value)
