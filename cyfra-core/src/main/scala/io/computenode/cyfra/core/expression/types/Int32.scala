package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Int32 extends SignedIntType
object Int32:
  def apply(value: Int): Int32 = const(value)
  given Value.Scalar[Int32] with
    protected def extractUnsafe(ir: ExpressionBlock[Int32]): Int32 = new Int32Impl(ir)
    def tag: Tag[Int32] = Tag[Int32]
  final class Int32Impl(val block: ExpressionBlock[Int32]) extends Int32 with ExpressionHolder[Int32]
