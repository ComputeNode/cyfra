package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{Operator, Expression, ExpressionBlock, Value}

trait Vec1Ops[T <: Scalar: Value, CC: Value]:
  self: CC =>
  def x: T = extract[T, CC](self, 0)
  def xx: Vec2[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec2[T]](self, self, Literal(0, 0))

private[ops] def extract[T: Value, CC: Value](cc: CC, i: Int): T =
  val s = Value[CC].peel(cc)
  Value[T].extract(s.add(Expression.Extract(s.result, i)))

private[ops] def insert[T: Value, CC: Value](cc: CC, v: T, i: Int): CC =
  val s = Value[CC].peel(cc)
  val sv = Value[T].peel(v)
  Value[CC].extract(s.extend(sv).add(Expression.Insert(s.result, sv.result, i)))
