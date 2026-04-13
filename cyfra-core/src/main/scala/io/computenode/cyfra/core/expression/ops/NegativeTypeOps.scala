package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

trait NegativeTypeOps[T <: NegativeType: Value] extends NumericalOps[T]:
  this: T =>
  private val self: T = this

  @targetName("neg")
  def unary_- : T = Value.map(Operator.Neg)(self)
  @targetName("rem")
  infix def rem(that: T): T = Value.map(Operator.Rem)(self, that)
