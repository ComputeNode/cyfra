package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.{BuildInFunction, Value}
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.Tag

import scala.annotation.targetName

//trait EqualOps[T]
//
//given [T <: Scalar: Value]: EqualOps[T] with {}
//given [T <: Scalar: Value]: EqualOps[Vec2[T]] with {}
//given [T <: Scalar: Value]: EqualOps[Vec3[T]] with {}
//given [T <: Scalar: Value]: EqualOps[Vec4[T]] with {}
//
//extension [T: {EqualOps, Value}](self: T)
//  @targetName("equal")
//  def ===(that: T): Bool =
//    if Value[T].bottomComposite.tag =:= Tag[Bool] then self.map[T, Bool](that)(BuildInFunction.LogicalEqual)
//    else self.map[T, Bool](that)(BuildInFunction.Equal)
//
//  @targetName("notEqual")
//  def !==(that: T): Bool =
//    if Value[T].bottomComposite.tag =:= Tag[Bool] then self.map[T, Bool](that)(BuildInFunction.LogicalNotEqual)
//    else self.map[T, Bool](that)(BuildInFunction.NotEqual)

// Logical operations on booleans
given BooleanOps[Bool] with {}
given BooleanOps[Vec2[Bool]] with {}
given BooleanOps[Vec3[Bool]] with {}
given BooleanOps[Vec4[Bool]] with {}

trait BooleanOps[T]

extension [T: {BooleanOps, Value}](self: T)
  @targetName("logicalOr")
  def ||(that: T): T = Value.map(BuildInFunction.LogicalOr)(self, that)

  @targetName("logicalAnd")
  def &&(that: T): T = Value.map(BuildInFunction.LogicalAnd)(self, that)

  @targetName("logicalNot")
  def unary_! : T = Value.map(BuildInFunction.LogicalNot)(self)

//  @targetName("logicalEqual")
//  def ===(that: T): T = self.map(that)(BuildInFunction.LogicalEqual)
//
//  @targetName("logicalNotEqual")
//  def !==(that: T): T = self.map(that)(BuildInFunction.LogicalNotEqual)

extension [V <: Vec[Bool]: Value](self: V)
  def any: Bool = Value.map(BuildInFunction.LogicalAny)(self)

  def all: Bool = Value.map(BuildInFunction.LogicalAll)(self)

// Floating-point checks
given [T <: FloatType: Value]: FloatCheckOps[T] with {}
given [T <: FloatType: Value]: FloatCheckOps[Vec2[T]] with {}
given [T <: FloatType: Value]: FloatCheckOps[Vec3[T]] with {}
given [T <: FloatType: Value]: FloatCheckOps[Vec4[T]] with {}

trait FloatCheckOps[T]

extension [T: {FloatCheckOps, Value}](self: T)
  def isNan: Bool = Value.map(BuildInFunction.IsNan)(self)

  def isInf: Bool = Value.map(BuildInFunction.IsInf)(self)

  def isFinite: Bool = Value.map(BuildInFunction.IsFinite)(self)

  def isNormal: Bool = Value.map(BuildInFunction.IsNormal)(self)

  def signBitSet: Bool = Value.map(BuildInFunction.SignBitSet)(self)

// Unified comparisons (works for floats, signed ints, and unsigned ints)
// Type detection happens later in the program, floats use ordered operations
given [T <: NumericalType: Value]: ComparisonOps[T] with {}
given [T <: NumericalType: Value]: ComparisonOps[Vec2[T]] with {}
given [T <: NumericalType: Value]: ComparisonOps[Vec3[T]] with {}
given [T <: NumericalType: Value]: ComparisonOps[Vec4[T]] with {}

trait ComparisonOps[T]

extension [T: {ComparisonOps, Value}](self: T)
  @targetName("equal")
  def ===(that: T): Bool = Value.map(BuildInFunction.Equal)(self, that)

  @targetName("notEqual")
  def !==(that: T): Bool = Value.map(BuildInFunction.NotEqual)(self, that)

  @targetName("lessThan")
  def <(that: T): Bool = Value.map(BuildInFunction.LessThan)(self, that)

  @targetName("greaterThan")
  def >(that: T): Bool = Value.map(BuildInFunction.GreaterThan)(self, that)

  @targetName("lessThanEqual")
  def <=(that: T): Bool = Value.map(BuildInFunction.LessThanEqual)(self, that)

  @targetName("greaterThanEqual")
  def >=(that: T): Bool = Value.map(BuildInFunction.GreaterThanEqual)(self, that)

// Select operation
extension [T: Value](cond: Bool) def select(obj1: T, obj2: T): T = Value.map(BuildInFunction.Select)(cond, obj1, obj2)

extension [V <: Vec[Bool]: Value, T <: Vec[?]: Value](cond: V) def select(obj1: T, obj2: T): T = Value.map(BuildInFunction.Select)(cond, obj1, obj2)
