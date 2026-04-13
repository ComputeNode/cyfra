package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.FloatOps
import izumi.reflect.Tag

abstract class Float64 extends FloatType with FloatOps[Float64]
object Float64:
  final class Float64Impl(val block: ExpressionBlock[Float64]) extends Float64 with ExpressionHolder[Float64]

  given Value.Scalar[Float64] with
    protected def extractUnsafe(ir: ExpressionBlock[Float64]): Float64 = new Float64Impl(ir)
    def tag: Tag[Float64] = Tag[Float64]

  given Conversion[Double, Float64] with
    def apply(value: Double): Float64 = Float64(value)

  def apply(value: Double): Float64 = const(value)
