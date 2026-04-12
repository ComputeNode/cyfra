package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.{Operator, Expression, Value}

trait Vec2Ops[T <: Scalar: Value, CC: Value] extends Vec1Ops[T, CC]:
  self: CC =>
  def y: T = extract[T, CC](self, 1)
  def yy: Vec2[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec2[T]](self, self, Literal(1, 1))

  def xy: Vec2[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec2[T]](self, self, Literal(0, 1))
