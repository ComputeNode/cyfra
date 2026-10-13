package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.{Expression, Operator, Value}

import scala.annotation.targetName

trait VecFloatOps[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]], Value[Vec[Bool]]):
  this: Vec[T] =>
  private val self: Vec[T] = this

  @targetName("vectorTimesScalar")
  def *(scalar: T)(using T <:< FloatType): Vec[T] = Value.map(Operator.VectorTimesScalar)(self, scalar)

  @targetName("dotProduct")
  infix def dot(that: Vec[T]): T = Value.map(Operator.Dot)(self, that)

  def isNan(using T <:< FloatType): Vec[Bool] = Value.map(Operator.IsNan)(self)

  def isInf(using T <:< FloatType): Vec[Bool] = Value.map(Operator.IsInf)(self)

  def isFinite(using T <:< FloatType): Vec[Bool] = Value.map(Operator.IsFinite)(self)

  def isNormal(using T <:< FloatType): Vec[Bool] = Value.map(Operator.IsNormal)(self)

  def signBitSet(using T <:< FloatType): Vec[Bool] = Value.map(Operator.SignBitSet)(self)
