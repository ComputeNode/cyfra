package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

import scala.annotation.targetName

given [T <: IntegerType: Value]: BitwiseOps[T] with {}
given [T <: IntegerType: Value]: BitwiseOps[Vec2[T]] with {}
given [T <: IntegerType: Value]: BitwiseOps[Vec3[T]] with {}
given [T <: IntegerType: Value]: BitwiseOps[Vec4[T]] with {}

trait BitwiseOps[T]

extension [T: {BitwiseOps, Value}](self: T)
  @targetName("shiftRightLogical")
  infix def >>>(shift: T): T = Value.map(BuildInFunction.ShiftRightLogical)(self, shift)

  @targetName("shiftRightArithmetic")
  infix def >>(shift: T): T = Value.map(BuildInFunction.ShiftRightArithmetic)(self, shift)

  @targetName("shiftLeftLogical")
  infix def <<(shift: T): T = Value.map(BuildInFunction.ShiftLeftLogical)(self, shift)

  @targetName("bitwiseOr")
  def |(that: T): T = Value.map(BuildInFunction.BitwiseOr)(self, that)

  @targetName("bitwiseXor")
  def ^(that: T): T = Value.map(BuildInFunction.BitwiseXor)(self, that)

  @targetName("bitwiseAnd")
  def &(that: T): T = Value.map(BuildInFunction.BitwiseAnd)(self, that)

  @targetName("bitwiseNot")
  def unary_~ : T = Value.map(BuildInFunction.BitwiseNot)(self)

  def bitFieldInsert[Offset: Value, Count: Value](insert: T, offset: Offset, count: Count): T =
    Value.map(BuildInFunction.BitFieldInsert)[T, T, Offset, Count, T](self, insert, offset, count)

  def bitFieldExtract[Offset: Value, Count: Value](offset: Offset, count: Count): T =
    Value.map(BuildInFunction.BitFieldExtract)[T, Offset, Count, T](self, offset, count)

  def bitReverse: T = Value.map(BuildInFunction.BitReverse)(self)

  def bitCount: T = Value.map(BuildInFunction.BitCount)(self)
