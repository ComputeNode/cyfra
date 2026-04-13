package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.NegativeTypeOps
import izumi.reflect.Tag

abstract class Int16 extends SignedIntType with NegativeTypeOps[Int16]
object Int16:
  final class Int16Impl(val block: ExpressionBlock[Int16]) extends Int16 with ExpressionHolder[Int16]

  given Value.Scalar[Int16] with
    protected def extractUnsafe(ir: ExpressionBlock[Int16]): Int16 = new Int16Impl(ir)
    def tag: Tag[Int16] = Tag[Int16]

  def apply(value: Int): Int16 = const(value)
