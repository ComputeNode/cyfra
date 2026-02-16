package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.{*, given}
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

trait Vec1Ops[T <: Scalar: Value, CC: Value]:
  self: CC =>
  def x: T = Value.map[CC, Literal, T](self, Literal(0))(BuildInFunction.CompositeExtract)
  def xx: Vec2[T] = Value.map[CC, Literal, Vec2[T]](self, Literal(0, 0))(BuildInFunction.CompositeExtract)
  def xxx: Vec3[T] = Value.map[CC, Literal, Vec3[T]](self, Literal(0, 0, 0))(BuildInFunction.CompositeExtract)
  def xxxx: Vec4[T] = Value.map[CC, Literal, Vec4[T]](self, Literal(0, 0, 0, 0))(BuildInFunction.CompositeExtract)
