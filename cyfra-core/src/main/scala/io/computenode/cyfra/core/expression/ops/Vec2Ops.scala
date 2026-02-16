package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Literal.given
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

trait Vec2Ops[T <: Scalar: Value, CC: Value] extends Vec1Ops[T, CC]:
  self: CC =>
  def y: T = Value.map[CC, Literal, T](self, Literal(1))(BuildInFunction.CompositeExtract)
  def yy: Vec2[T] = Value.map[CC, CC,Literal, Vec2[T]](self, self, Literal(1, 1))(BuildInFunction.VectorShuffle)

  def xy: Vec2[T] = Value.map[CC, CC,Literal, Vec2[T]](self, self, Literal(0, 1))(BuildInFunction.VectorShuffle)
