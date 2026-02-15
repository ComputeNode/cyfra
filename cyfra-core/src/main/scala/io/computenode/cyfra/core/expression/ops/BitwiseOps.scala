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
  infix def >>>(shift: T): T = Value.map(self, shift)(BuildInFunction.ShiftRightLogical)

  @targetName("shiftRightArithmetic")
  infix def >>(shift: T): T = Value.map(self, shift)(BuildInFunction.ShiftRightArithmetic)

  @targetName("shiftLeftLogical")
  infix def <<(shift: T): T = Value.map(self, shift)(BuildInFunction.ShiftLeftLogical)

  @targetName("bitwiseOr")
  def |(that: T): T = Value.map(self, that)(BuildInFunction.BitwiseOr)

  @targetName("bitwiseXor")
  def ^(that: T): T = Value.map(self, that)(BuildInFunction.BitwiseXor)

  @targetName("bitwiseAnd")
  def &(that: T): T = Value.map(self, that)(BuildInFunction.BitwiseAnd)

  @targetName("bitwiseNot")
  def unary_~ : T = Value.map(self)(BuildInFunction.BitwiseNot)

  def bitFieldInsert[Offset: Value, Count: Value](insert: T, offset: Offset, count: Count): T =
    Value.map[T, T, Offset, Count, T](self, insert, offset, count)(BuildInFunction.BitFieldInsert)

  def bitFieldExtract[Offset: Value, Count: Value](offset: Offset, count: Count): T =
    Value.map[T, Offset, Count, T](self, offset, count)(BuildInFunction.BitFieldExtract)

  def bitReverse: T = Value.map(self)(BuildInFunction.BitReverse)

  def bitCount: T = Value.map(self)(BuildInFunction.BitCount)
