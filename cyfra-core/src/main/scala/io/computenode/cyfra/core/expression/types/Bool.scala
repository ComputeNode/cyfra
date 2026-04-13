package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Bool extends BoolType
object Bool:
  def apply(value: Boolean): Bool = const(value)
  given Value.Scalar[Bool] with
    protected def extractUnsafe(ir: ExpressionBlock[Bool]): Bool = new BoolImpl(ir)
    def tag: Tag[Bool] = Tag[Bool]
  final class BoolImpl(val block: ExpressionBlock[Bool]) extends Bool with ExpressionHolder[Bool]
