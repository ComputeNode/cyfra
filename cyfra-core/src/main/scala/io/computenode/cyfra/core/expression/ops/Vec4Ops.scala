package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{Operator, Value}

trait Vec4Ops[T <: Scalar: Value, CC: Value] extends Vec3Ops[T, CC]:
  self: CC =>
  def w: T = extract[T, CC](self, 3)
  def ww: Vec2[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec2[T]](self, self, Literal(3, 3))

  def xyzw: Vec4[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec4[T]](self, self, Literal(0, 1, 2, 3))
  def zxwx: Vec4[T] = Value.map(Operator.VectorShuffle)[CC, CC, Literal, Vec4[T]](self, self, Literal(2, 0, 3, 0))
