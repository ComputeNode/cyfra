package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.Bool
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

trait BoolOps extends ScalarOps[Bool]:
  this: Bool =>
  private val self: Bool = this

  @targetName("logicalOr")
  def ||(that: Bool): Bool = Value.map(Operator.LogicalOr)(self, that)

  @targetName("logicalAnd")
  def &&(that: Bool): Bool = Value.map(Operator.LogicalAnd)(self, that)

  @targetName("logicalNot")
  def unary_! : Bool = Value.map(Operator.LogicalNot)(self)
