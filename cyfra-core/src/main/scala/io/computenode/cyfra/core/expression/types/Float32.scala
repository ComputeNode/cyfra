package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.FloatOps
import izumi.reflect.Tag

abstract class Float32 extends FloatType with FloatOps[Float32]
object Float32:
  final class Float32Impl(val block: ExpressionBlock[Float32]) extends Float32 with ExpressionHolder[Float32]

  given Value.Scalar[Float32] with
    protected def extractUnsafe(ir: ExpressionBlock[Float32]): Float32 = new Float32Impl(ir)
    def tag: Tag[Float32] = Tag[Float32]

  given Conversion[Float, Float32] with
    def apply(value: Float): Float32 = Float32(value)

  def apply(value: Float): Float32 = const(value)
