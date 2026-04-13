package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.{Expression, Operator, Value}

import scala.annotation.targetName

trait VecIntegerOps[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]]):
  this: Vec[T] =>
  private val self: Vec[T] = this

  @targetName("shiftRightLogical")
  infix def >>>(shift: Vec[T])(using T <:< IntegerType): Vec[T] = Value.map(Operator.ShiftRightLogical)(self, shift)

  @targetName("shiftRightArithmetic")
  infix def >>(shift: Vec[T])(using T <:< IntegerType): Vec[T] = Value.map(Operator.ShiftRightArithmetic)(self, shift)

  @targetName("shiftLeftLogical")
  infix def <<(shift: Vec[T])(using T <:< IntegerType): Vec[T] = Value.map(Operator.ShiftLeftLogical)(self, shift)

  @targetName("bitwiseOr")
  def |(that: Vec[T])(using T <:< IntegerType): Vec[T] = Value.map(Operator.BitwiseOr)(self, that)

  @targetName("bitwiseXor")
  def ^(that: Vec[T])(using T <:< IntegerType): Vec[T] = Value.map(Operator.BitwiseXor)(self, that)

  @targetName("bitwiseAnd")
  def &(that: Vec[T])(using T <:< IntegerType): Vec[T] = Value.map(Operator.BitwiseAnd)(self, that)

  @targetName("bitwiseNot")
  def unary_~(using T <:< IntegerType): Vec[T] = Value.map(Operator.BitwiseNot)(self)

  def bitFieldInsert[Offset <: IntegerType: Value, Count <: IntegerType: Value](insert: Vec[T], offset: Offset, count: Count)(using
    T <:< IntegerType,
  ): Vec[T] =
    Value.map(Operator.BitFieldInsert)(self, insert, offset, count)

  def bitFieldExtract[Offset <: IntegerType: Value, Count <: IntegerType: Value](offset: Offset, count: Count)(using T <:< IntegerType): Vec[T] =
    Value.map(Operator.BitFieldExtract)(self, offset, count)

  def bitReverse(using T <:< IntegerType): Vec[T] = Value.map(Operator.BitReverse)(self)

  def bitCount(using T <:< IntegerType): Vec[T] = Value.map(Operator.BitCount)(self)
