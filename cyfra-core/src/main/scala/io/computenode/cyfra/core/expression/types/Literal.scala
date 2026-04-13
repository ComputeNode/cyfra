package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.{Expression, ExpressionBlock, ExpressionHolder, Value}
import izumi.reflect.Tag

/** This is a special type that inlines itself into the spir-v command
  *
  * Use only when directly interacting with Expression
  */
abstract class Literal

object Literal:
  private class LiteralImpl(val block: ExpressionBlock[Literal]) extends Literal with ExpressionHolder[Literal]

  def apply(ints: Int*): Literal = Value[Literal].extract(ExpressionBlock(Expression.LiteralArgs(ints.toList)))

  given Value.Scalar[Literal] with
    protected def extractUnsafe(ir: ExpressionBlock[Literal]): Literal = new LiteralImpl(ir)
    def tag: Tag[Literal] = Tag[Literal]
