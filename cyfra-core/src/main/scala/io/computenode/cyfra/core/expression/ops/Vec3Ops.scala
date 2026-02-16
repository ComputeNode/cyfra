package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

trait Vec3Ops[T <: Scalar: Value, CC: Value] extends Vec2Ops[T, CC]:
  self: CC =>
  def z: T = Value.map[CC, Literal, T](self, Literal(2))(BuildInFunction.CompositeExtract)
  def zz: Vec2[T] = Value.map[CC, Literal, Vec2[T]](self, Literal(2, 2))(BuildInFunction.CompositeExtract)
  def zzy: Vec3[T] = Value.map[CC, Literal, Vec3[T]](self, Literal(2, 2, 2))(BuildInFunction.CompositeExtract)
  def zzzz: Vec4[T] = Value.map[CC, Literal, Vec4[T]](self, Literal(2, 2, 2, 2))(BuildInFunction.CompositeExtract)

  def xyz: Vec3[T] = Value.map[CC, Literal, Vec3[T]](self, Literal(0, 1, 2))(BuildInFunction.CompositeExtract)
