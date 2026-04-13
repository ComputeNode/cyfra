package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.BoolOps
import izumi.reflect.Tag

abstract class Bool extends BoolType with BoolOps
object Bool:
  final class BoolImpl(val block: ExpressionBlock[Bool]) extends Bool with ExpressionHolder[Bool]

  given Value.Scalar[Bool] with
    protected def extractUnsafe(ir: ExpressionBlock[Bool]): Bool = new BoolImpl(ir)
    def tag: Tag[Bool] = Tag[Bool]

  def apply(value: Boolean): Bool = const(value)
