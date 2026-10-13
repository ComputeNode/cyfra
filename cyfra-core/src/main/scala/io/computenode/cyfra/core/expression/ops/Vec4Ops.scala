package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import VecOps.extract
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{Operator, Value}

trait Vec4Ops[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]]) extends Vec3Ops[T, Vec]:
  this: Vec[T] =>
  private val self: Vec[T] = this
 
  def w: T = extract(self, 3)
  def ww: Vec2[T] = Value.map(Operator.VectorShuffle)(self, self, Literal(3, 3))

  def xyzw: Vec4[T] = Value.map(Operator.VectorShuffle)(self, self, Literal(0, 1, 2, 3))
  def zxwx: Vec4[T] = Value.map(Operator.VectorShuffle)(self, self, Literal(2, 0, 3, 0))
