package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.{Expression, Operator, Value}

import scala.annotation.targetName

trait VecNegativeOps[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]]):
  this: Vec[T] =>
  private val self: Vec[T] = this

  @targetName("neg")
  def unary_-(using T <:< NegativeType): Vec[T] = Value.map(Operator.Neg)(self)

  @targetName("rem")
  infix def rem(that: Vec[T])(using T <:< NegativeType): Vec[T] = Value.map(Operator.Rem)(self, that)
