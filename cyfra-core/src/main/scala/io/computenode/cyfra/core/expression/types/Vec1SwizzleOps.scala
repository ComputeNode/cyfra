package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.VecOps.extract
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{Expression, ExpressionBlock, Operator, Value}

trait Vec1SwizzleOps[T <: Scalar: Value, CC: Value]:
  self: CC =>
  def x: T = extract[T, CC](self, 0)
  def xx: Vec2[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec2[T]](self, self, Literal(0, 0))

