package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.IntegerOps
import izumi.reflect.Tag

abstract class UInt32 extends UnsignedIntType with IntegerOps[UInt32]
object UInt32:
  final class UInt32Impl(val block: ExpressionBlock[UInt32]) extends UInt32 with ExpressionHolder[UInt32]

  given Conversion[Int, UInt32] with
    def apply(value: Int): UInt32 = UInt32(value)

  given Value.Scalar[UInt32] with
    protected def extractUnsafe(ir: ExpressionBlock[UInt32]): UInt32 = new UInt32Impl(ir)
    def tag: Tag[UInt32] = Tag[UInt32]

  def apply(value: Int): UInt32 = const(value)

  def apply(value: Int32): UInt32 = Value.map(Operator.ConvertToU)(value)
