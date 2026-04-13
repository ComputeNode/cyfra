package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import VecOps.extract
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{Expression, ExpressionBlock, Operator, Value}

trait Vec1Ops[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]]):
  this: Vec[T] =>
  private val self: Vec[T] = this

  def x: T = extract(self, 0)
  def xx: Vec2[T] = Value.map(Operator.VectorShuffle)(self, self, Literal(0, 0))
