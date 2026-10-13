package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import VecOps.{extract, insert}
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{Operator, Value}

trait Vec3Ops[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]]) extends Vec2Ops[T, Vec]:
  this: Vec[T] =>
  private val self: Vec[T] = this
  
  def z: T = extract(self, 2)

  def zz: Vec2[T] = Value.map(Operator.VectorShuffle)(self, self, Literal(2, 2))
  def yzx: Vec3[T] = Value.map(Operator.VectorShuffle)(self, self, Literal(1, 2, 0))
