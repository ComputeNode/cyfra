package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

trait Vec3Ops[T <: Scalar: Value, CC: Value] extends Vec2Ops[T, CC]:
  self: CC =>
  def z: T = Value.map(BuildInFunction.CompositeExtract)[CC, Literal, T](self, Literal(2))
  def z(value: T): CC = Value.map(BuildInFunction.CompositeInsert)[T, CC, Literal, CC](value, self, Literal(2))

  def zz: Vec2[T] = Value.map(BuildInFunction.VectorShuffle)[CC, CC, Literal, Vec2[T]](self, self, Literal(2, 2))
  def yzx: Vec3[T] = Value.map(BuildInFunction.VectorShuffle)[CC, CC, Literal, Vec3[T]](self, self, Literal(1, 2, 0))
