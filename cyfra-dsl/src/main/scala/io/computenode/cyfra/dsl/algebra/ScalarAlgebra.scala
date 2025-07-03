package io.computenode.cyfra.dsl.algebra

import io.computenode.cyfra.dsl.Expression.ConstFloat32
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.library.Functions.abs
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag

import scala.annotation.targetName

object ScalarAlgebra:

  trait BasicScalarAlgebra[T <: Scalar: FromExpr: Tag]
      extends ScalarSummable[T]
      with ScalarDiffable[T]
      with ScalarMulable[T]
      with ScalarDivable[T]
      with ScalarModable[T]
      with Comparable[T]
      with ScalarNegatable[T]

  trait BasicScalarIntAlgebra[T <: Scalar: FromExpr: Tag] extends BasicScalarAlgebra[T] with BitwiseOperable[T]

  given BasicScalarAlgebra[Float32] = new BasicScalarAlgebra[Float32] {}
  given BasicScalarIntAlgebra[Int32] = new BasicScalarIntAlgebra[Int32] {}
  given BasicScalarIntAlgebra[UInt32] = new BasicScalarIntAlgebra[UInt32] {}

  trait ScalarSummable[T <: Scalar: FromExpr: Tag]:
    def sum(a: T, b: T)(using name: Source): T = summon[FromExpr[T]].fromExpr(Sum(a, b))

  extension [T <: Scalar: ScalarSummable: Tag](a: T)
    @targetName("add")
    inline def +(b: T)(using Source): T = summon[ScalarSummable[T]].sum(a, b)

  trait ScalarDiffable[T <: Scalar: FromExpr: Tag]:
    def diff(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(Diff(a, b))

  extension [T <: Scalar: ScalarDiffable: Tag](a: T)
    @targetName("sub")
    inline def -(b: T)(using Source): T = summon[ScalarDiffable[T]].diff(a, b)

  // T and S ??? so two
  trait ScalarMulable[T <: Scalar: FromExpr: Tag]:
    def mul(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(Mul(a, b))

  extension [T <: Scalar: ScalarMulable: Tag](a: T)
    @targetName("mul")
    inline def *(b: T)(using Source): T = summon[ScalarMulable[T]].mul(a, b)

  trait ScalarDivable[T <: Scalar: FromExpr: Tag]:
    def div(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(Div(a, b))

  extension [T <: Scalar: ScalarDivable: Tag](a: T)
    @targetName("div")
    inline def /(b: T)(using Source): T = summon[ScalarDivable[T]].div(a, b)

  trait ScalarNegatable[T <: Scalar: FromExpr: Tag]:
    def negate(a: T)(using Source): T = summon[FromExpr[T]].fromExpr(Negate(a))

  extension [T <: Scalar: ScalarNegatable: Tag](a: T)
    @targetName("negate")
    inline def unary_-(using Source): T = summon[ScalarNegatable[T]].negate(a)

  trait ScalarModable[T <: Scalar: FromExpr: Tag]:
    def mod(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(Mod(a, b))

  extension [T <: Scalar: ScalarModable: Tag](a: T) inline infix def mod(b: T)(using Source): T = summon[ScalarModable[T]].mod(a, b)

  trait Comparable[T <: Scalar: FromExpr: Tag]:
    def greaterThan(a: T, b: T)(using Source): GBoolean = GBoolean(GreaterThan(a, b))

    def lessThan(a: T, b: T)(using Source): GBoolean = GBoolean(LessThan(a, b))

    def greaterThanEqual(a: T, b: T)(using Source): GBoolean = GBoolean(GreaterThanEqual(a, b))

    def lessThanEqual(a: T, b: T)(using Source): GBoolean = GBoolean(LessThanEqual(a, b))

    def equal(a: T, b: T)(using Source): GBoolean = GBoolean(Equal(a, b))

  extension [T <: Scalar: Comparable: Tag](a: T)
    inline def >(b: T)(using Source): GBoolean = summon[Comparable[T]].greaterThan(a, b)
    inline def <(b: T)(using Source): GBoolean = summon[Comparable[T]].lessThan(a, b)
    inline def >=(b: T)(using Source): GBoolean = summon[Comparable[T]].greaterThanEqual(a, b)
    inline def <=(b: T)(using Source): GBoolean = summon[Comparable[T]].lessThanEqual(a, b)
    inline def ===(b: T)(using Source): GBoolean = summon[Comparable[T]].equal(a, b)

  case class Epsilon(eps: Float)

  given Epsilon = Epsilon(0.00001f)

  extension (f32: Float32)
    inline def asInt(using Source): Int32 = Int32(ToInt32(f32))
    inline def =~=(other: Float32)(using epsilon: Epsilon): GBoolean =
      abs(f32 - other) < epsilon.eps

  extension (i32: Int32)
    inline def asFloat(using Source): Float32 = Float32(ToFloat32(i32))
    inline def unsigned(using Source): UInt32 = UInt32(ToUInt32(i32))

  extension (u32: UInt32)
    inline def asFloat(using Source): Float32 = Float32(ToFloat32(u32))
    inline def signed(using Source): Int32 = Int32(ToInt32(u32))

  trait BitwiseOperable[T <: Scalar: FromExpr: Tag]:
    def bitwiseAnd(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(BitwiseAnd(a, b))

    def bitwiseOr(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(BitwiseOr(a, b))

    def bitwiseXor(a: T, b: T)(using Source): T = summon[FromExpr[T]].fromExpr(BitwiseXor(a, b))

    def bitwiseNot(a: T)(using Source): T = summon[FromExpr[T]].fromExpr(BitwiseNot(a))

    def shiftLeft(a: T, by: UInt32)(using Source): T = summon[FromExpr[T]].fromExpr(ShiftLeft(a, by))

    def shiftRight(a: T, by: UInt32)(using Source): T = summon[FromExpr[T]].fromExpr(ShiftRight(a, by))

  extension [T <: Scalar: BitwiseOperable: Tag](a: T)
    inline def &(b: T)(using Source): T = summon[BitwiseOperable[T]].bitwiseAnd(a, b)
    inline def |(b: T)(using Source): T = summon[BitwiseOperable[T]].bitwiseOr(a, b)
    inline def ^(b: T)(using Source): T = summon[BitwiseOperable[T]].bitwiseXor(a, b)
    inline def unary_~(using Source): T = summon[BitwiseOperable[T]].bitwiseNot(a)
    inline def <<(by: UInt32)(using Source): T = summon[BitwiseOperable[T]].shiftLeft(a, by)
    inline def >>(by: UInt32)(using Source): T = summon[BitwiseOperable[T]].shiftRight(a, by)

  given (using Source): Conversion[Float, Float32] = f => Float32(ConstFloat32(f))
  given (using Source): Conversion[Int, Int32] = i => Int32(ConstInt32(i))
  given (using Source): Conversion[Int, UInt32] = i => UInt32(ConstUInt32(i))
  given (using Source): Conversion[Boolean, GBoolean] = b => GBoolean(ConstGB(b))

  type FloatOrFloat32 = Float | Float32

  inline def toFloat32(f: FloatOrFloat32)(using Source): Float32 = f match
    case f: Float   => Float32(ConstFloat32(f))
    case f: Float32 => f

  extension (b: GBoolean)
    def &&(other: GBoolean)(using Source): GBoolean = GBoolean(And(b, other))
    def ||(other: GBoolean)(using Source): GBoolean = GBoolean(Or(b, other))
    def unary_!(using Source): GBoolean = GBoolean(Not(b))
