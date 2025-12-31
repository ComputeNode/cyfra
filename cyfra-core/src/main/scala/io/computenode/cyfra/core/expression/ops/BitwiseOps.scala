package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}

import scala.annotation.targetName

given [T <: IntegerType: Value]: BitwiseOps[T] with {}
given [T <: IntegerType: Value]: BitwiseOps[Vec2[T]] with {}
given [T <: IntegerType: Value]: BitwiseOps[Vec3[T]] with {}
given [T <: IntegerType: Value]: BitwiseOps[Vec4[T]] with {}

trait BitwiseOps[T]

extension [T: {BitwiseOps, Value}](self: T)
  @targetName("shiftRightLogical")
  infix def >>>(shift: T): T = self.map(shift)(BuildInFunction.ShiftRightLogical)
  
  @targetName("shiftRightArithmetic")
  infix def >>(shift: T): T = self.map(shift)(BuildInFunction.ShiftRightArithmetic)
  
  @targetName("shiftLeftLogical")
  infix def <<(shift: T): T = self.map(shift)(BuildInFunction.ShiftLeftLogical)
  
  @targetName("bitwiseOr")
  def |(that: T): T = self.map(that)(BuildInFunction.BitwiseOr)
  
  @targetName("bitwiseXor")
  def ^(that: T): T = self.map(that)(BuildInFunction.BitwiseXor)
  
  @targetName("bitwiseAnd")
  def &(that: T): T = self.map(that)(BuildInFunction.BitwiseAnd)
  
  @targetName("bitwiseNot")
  def unary_~ : T = self.map(BuildInFunction.BitwiseNot)
  
  def bitFieldInsert[Offset: Value, Count: Value](insert: T, offset: Offset, count: Count): T =
    self.map[T, Offset, Count, T](insert, offset, count)(BuildInFunction.BitFieldInsert)
  
  def bitFieldExtract[Offset: Value, Count: Value](offset: Offset, count: Count): T =
    self.map[Offset, Count, T](offset, count)(BuildInFunction.BitFieldExtract)
  
  def bitReverse: T = self.map(BuildInFunction.BitReverse)
  
  def bitCount: T = self.map[T](BuildInFunction.BitCount)

