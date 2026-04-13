package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import VecOps.{extract, insert}
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{Operator, Value}

trait Vec3Ops[T <: Scalar: Value, CC: Value] extends Vec2Ops[T, CC]:
  self: CC =>
  def z: T = extract[T, CC](self, 2)
  def z(value: T): CC = insert[T, CC](self, value, 2)

  def zz: Vec2[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec2[T]](self, self, Literal(2, 2))
  def yzx: Vec3[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec3[T]](self, self, Literal(1, 2, 0))
