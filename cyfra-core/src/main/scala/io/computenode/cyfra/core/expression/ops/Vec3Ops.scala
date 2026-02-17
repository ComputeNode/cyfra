package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

trait Vec3Ops[T <: Scalar: Value, CC: Value] extends Vec2Ops[T, CC]:
  self: CC =>
  def z: T = Value.map[CC, Literal, T](self, Literal(2))(BuildInFunction.CompositeExtract)
  def z(value: T): CC = Value.map[T, CC, Literal, CC](value, self, Literal(2))(BuildInFunction.CompositeInsert)

  def zz: Vec2[T] = Value.map[CC, CC, Literal, Vec2[T]](self, self, Literal(2, 2))(BuildInFunction.VectorShuffle)
  def yzx: Vec3[T] = Value.map[CC, CC, Literal, Vec3[T]](self, self, Literal(1, 2, 0))(BuildInFunction.VectorShuffle)
