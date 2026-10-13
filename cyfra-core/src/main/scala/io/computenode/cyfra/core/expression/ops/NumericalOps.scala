package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.{Bool, NumericalType, Vec}
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

trait NumericalOps[T <: NumericalType: Value] extends ScalarOps[T]:
  this: T =>
  private val self: T = this

  @targetName("lessThan")
  def <(that: T): Bool = Value.map(Operator.LessThan)(self, that)

  @targetName("greaterThan")
  def >(that: T): Bool = Value.map(Operator.GreaterThan)(self, that)

  @targetName("lessThanEqual")
  def <=(that: T): Bool = Value.map(Operator.LessThanEqual)(self, that)

  @targetName("greaterThanEqual")
  def >=(that: T): Bool = Value.map(Operator.GreaterThanEqual)(self, that)

  @targetName("add")
  def +(that: T): T = Value.map(Operator.Add)(self, that)

  @targetName("sub")
  def -(that: T): T = Value.map(Operator.Sub)(self, that)

  @targetName("mul")
  def *(that: T): T = Value.map(Operator.Mul)(self, that)

  @targetName("div")
  def /(that: T): T = Value.map(Operator.Div)(self, that)

  @targetName("mod")
  def %(that: T): T = Value.map(Operator.Mod)(self, that)

