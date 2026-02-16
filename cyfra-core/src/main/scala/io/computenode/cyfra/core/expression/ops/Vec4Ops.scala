package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

trait Vec4Ops[T <: Scalar: Value, CC: Value] extends Vec3Ops[T, CC]:
  self: CC =>
  def w: T = Value.map[CC, Literal, T](self, Literal(3))(BuildInFunction.CompositeExtract)
  def ww: Vec2[T] = Value.map[CC, Literal, Vec2[T]](self, Literal(3, 3))(BuildInFunction.CompositeExtract)
  def www: Vec3[T] = Value.map[CC, Literal, Vec3[T]](self, Literal(3, 3, 3))(BuildInFunction.CompositeExtract)
  def wwww: Vec4[T] = Value.map[CC, Literal, Vec4[T]](self, Literal(3, 3, 3, 3))(BuildInFunction.CompositeExtract)

  def xyzw: Vec4[T] = Value.map[CC, Literal, Vec4[T]](self, Literal(0, 1, 2, 3))(BuildInFunction.CompositeExtract)
  def zxwx: Vec4[T] = Value.map[CC, Literal, Vec4[T]](self, Literal(2, 0, 3, 0))(BuildInFunction.CompositeExtract)
