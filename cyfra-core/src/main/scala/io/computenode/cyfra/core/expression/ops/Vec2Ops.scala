package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import VecOps.extract
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{Expression, Operator, Value}

trait Vec2Ops[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]]) extends Vec1Ops[T, Vec]:
  this: Vec[T] =>
  private val self: Vec[T] = this
  
  def y: T = extract(self, 1)
  def yy: Vec2[T] = Value.map(Operator.VectorShuffle)(self, self, Literal(1, 1))

  def xy: Vec2[T] = Value.map(Operator.VectorShuffle)(self, self, Literal(0, 1))
