package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.{Expression, Operator, Value}

import scala.annotation.targetName

trait VecBoolOps[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]], Value[Vec[Bool]]):
  this: Vec[T] =>
  private val self: Vec[T] = this

  @targetName("logicalOr")
  def ||(that: Vec[T])(using T =:= Bool): Vec[Bool] = Value.map(Operator.LogicalOr)(self, that)

  @targetName("logicalAnd")
  def &&(that: Vec[T])(using T =:= Bool): Vec[Bool] = Value.map(Operator.LogicalAnd)(self, that)

  @targetName("logicalNot")
  def unary_!(using T =:= Bool): Vec[Bool] = Value.map(Operator.LogicalNot)(self)

  def any(using T =:= Bool): Bool = Value.map(Operator.LogicalAny)(self)

  def all(using T =:= Bool): Bool = Value.map(Operator.LogicalAll)(self)
