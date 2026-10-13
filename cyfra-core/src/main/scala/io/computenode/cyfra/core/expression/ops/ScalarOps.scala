package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.{Bool, NumericalType, Scalar}
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

trait ScalarOps[T <: Scalar: Value]:
  this: T =>
  private val self: T = this

  @targetName("equal")
  def ===(that: T): Bool = Value.map(Operator.Equal)(self, that)

  @targetName("notEqual")
  def !==(that: T): Bool = Value.map(Operator.NotEqual)(self, that)
