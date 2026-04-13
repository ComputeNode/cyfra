package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.{Expression, Operator, Value}

import scala.annotation.targetName

trait VecOps[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]], Value[Vec[Bool]]):
  this: Vec[T] =>
  val self: Vec[T] = this

  @targetName("equal")
  def ===(that: Vec[T]): Vec[Bool] = Value.map(Operator.Equal)(self, that)

  @targetName("notEqual")
  def !==(that: Vec[T]): Vec[Bool] = Value.map(Operator.NotEqual)(self, that)

  // numerical ops
  @targetName("lessThan")
  def <(that: Vec[T])(using T <:< NumericalType): Vec[Bool] = Value.map(Operator.LessThan)(self, that)

  @targetName("greaterThan")
  def >(that: Vec[T])(using T <:< NumericalType): Vec[Bool] = Value.map(Operator.GreaterThan)(self, that)

  @targetName("lessThanEqual")
  def <=(that: Vec[T])(using T <:< NumericalType): Vec[Bool] = Value.map(Operator.LessThanEqual)(self, that)

  @targetName("greaterThanEqual")
  def >=(that: Vec[T])(using T <:< NumericalType): Vec[Bool] = Value.map(Operator.GreaterThanEqual)(self, that)

  @targetName("add")
  def +(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Add)(self, that)

  @targetName("sub")
  def -(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Sub)(self, that)

  @targetName("mul")
  def *(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Mul)(self, that)

  @targetName("div")
  def /(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Div)(self, that)

  @targetName("mod")
  def %(that: Vec[T])(using T <:< NumericalType): Vec[T] = Value.map(Operator.Mod)(self, that)

  // negative ops
  @targetName("neg")
  def unary_-(using T <:< NegativeType): Vec[T] = Value.map(Operator.Neg)(self)

  @targetName("rem")
  infix def rem(that: Vec[T])(using T <:< NegativeType): Vec[T] = Value.map(Operator.Rem)(self, that)

  // integer ops
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

  // floating ops
  @targetName("vectorTimesScalar")
  def *(scalar: T)(using T <:< FloatType): Vec[T] = Value.map(Operator.VectorTimesScalar)(self, scalar)

  def isNan(using T <:< FloatType): Vec[Bool] = Value.map(Operator.IsNan)(self)

  def isInf(using T <:< FloatType): Vec[Bool] = Value.map(Operator.IsInf)(self)

  def isFinite(using T <:< FloatType): Vec[Bool] = Value.map(Operator.IsFinite)(self)

  def isNormal(using T <:< FloatType): Vec[Bool] = Value.map(Operator.IsNormal)(self)

  def signBitSet(using T <:< FloatType): Vec[Bool] = Value.map(Operator.SignBitSet)(self)

  // boolean ops
  @targetName("logicalOr")
  def ||(that: Vec[T])(using T =:= Bool): Vec[T] = Value.map(Operator.LogicalOr)(self, that)

  @targetName("logicalAnd")
  def &&(that: Vec[T])(using T =:= Bool): Vec[T] = Value.map(Operator.LogicalAnd)(self, that)

  @targetName("logicalNot")
  def unary_!(using T =:= Bool): Vec[T] = Value.map(Operator.LogicalNot)(self)

  def any(using T =:= Bool): Bool = Value.map(Operator.LogicalAny)(self)

  def all(using T =:= Bool): Bool = Value.map(Operator.LogicalAll)(self)

object VecOps:
  private[types] def extract[T: Value, CC: Value](cc: CC, i: Int): T =
    val s = Value[CC].peel(cc)
    Value[T].extract(s.add(Expression.Extract(s.result, i)))

  private[types] def insert[T: Value, CC: Value](cc: CC, v: T, i: Int): CC =
    val s = Value[CC].peel(cc)
    val sv = Value[T].peel(v)
    Value[CC].extract(s.extend(sv).add(Expression.Insert(s.result, sv.result, i)))
