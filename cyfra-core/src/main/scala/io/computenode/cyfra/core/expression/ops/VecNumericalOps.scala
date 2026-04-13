package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.{Expression, Operator, Value}

import scala.annotation.targetName

trait VecNumericalOps[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]], Value[Vec[Bool]]):
  this: Vec[T] =>
  private val self: Vec[T] = this

  @targetName("lessThan")
  def <(that: Vec[T])(using T <:< NumericalType): Vec[Bool] = Value.map(Operator.LessThan)(self, that)

  @targetName("greaterThan")
  def >(that: Vec[T])(using T <:< NumericalType): Vec[Bool] = Value.map(Operator.GreaterThan)(self, that)

  @targetName("lessThanEqual")
  def <=(that: Vec[T])(using T <:< NumericalType): Vec[Bool] = Value.map(Operator.LessThanEqual)(self, that)

  @targetName("greaterThanEqual")
  def >=(that: Vec[T])(using T <:< NumericalType): Vec[Bool] = Value.map(Operator.GreaterThanEqual)(self, that)

  @targetName("add")
  def +(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Add)(self, that)

  @targetName("sub")
  def -(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Sub)(self, that)

  @targetName("mul")
  def *(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Mul)(self, that)

  @targetName("div")
  def /(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Div)(self, that)

  @targetName("mod")
  def %(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Mod)(self, that)
