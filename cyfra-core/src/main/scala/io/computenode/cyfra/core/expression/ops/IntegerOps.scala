package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

trait IntegerOps[T <: IntegerType: Value] extends NumericalOps[T]:
  this: T =>
  private val self: T = this

  @targetName("shiftRightLogical")
  infix def >>>(shift: T): T = Value.map(Operator.ShiftRightLogical)(self, shift)

  @targetName("shiftRightArithmetic")
  infix def >>(shift: T): T = Value.map(Operator.ShiftRightArithmetic)(self, shift)

  @targetName("shiftLeftLogical")
  infix def <<(shift: T): T = Value.map(Operator.ShiftLeftLogical)(self, shift)

  @targetName("bitwiseOr")
  def |(that: T): T = Value.map(Operator.BitwiseOr)(self, that)

  @targetName("bitwiseXor")
  def ^(that: T): T = Value.map(Operator.BitwiseXor)(self, that)

  @targetName("bitwiseAnd")
  def &(that: T): T = Value.map(Operator.BitwiseAnd)(self, that)

  @targetName("bitwiseNot")
  def unary_~ : T = Value.map(Operator.BitwiseNot)(self)

  def bitFieldInsert[Offset <: IntegerType: Value, Count <: IntegerType: Value](insert: T, offset: Offset, count: Count): T =
    Value.map(Operator.BitFieldInsert)(self, insert, offset, count)

  def bitFieldExtract[Offset <: IntegerType: Value, Count <: IntegerType: Value](offset: Offset, count: Count): T =
    Value.map(Operator.BitFieldExtract)(self, offset, count)

  def bitReverse: T = Value.map(Operator.BitReverse)(self)

  def bitCount: T = Value.map(Operator.BitCount)(self)
