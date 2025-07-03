package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import izumi.reflect.Tag

object Simulate:
  def sim(e: Expression[?]): Any = e match
    case e: PhantomExpression[?]      => ???
    case Negate(a)                    => -simValue(a)
    case e: BinaryOpExpression[?]     => simBinOp(e)
    case ScalarProd(a, b)             => ???
    case DotProd(a, b)                => ???
    case e: BitwiseOpExpression[?]    => simBitwiseOp(e)
    case e: ComparisonOpExpression[?] => simCompareOp(e)
    case And(a, b)                    => simScalar(a) && simScalar(b)
    case Or(a, b)                     => simScalar(a) || simScalar(b)
    case Not(a)                       => !simScalar(a)
    case ExtractScalar(a, i)          => ???
    case e: ConvertExpression[?, ?]   => simConvert(e)
    case e: Const[?]                  => simConst(e)
    case ComposeVec2(a, b)            => ???
    case ComposeVec3(a, b, c)         => ???
    case ComposeVec4(a, b, c, d)      => ???
    case ExtFunctionCall(fn, args)    => ???
    case FunctionCall(fn, body, args) => ???
    case InvocationId                 => ???
    case Pass(value)                  => ???
    case Dynamic(source)              => ???
    case _                            => throw IllegalArgumentException("wrong argument")

  def simBinOp(e: BinaryOpExpression[?]): Any = e match
    case Sum(a, b)  => sim(a.tree) + sim(b.tree)
    case Diff(a, b) => sim(a.tree) - sim(b.tree)
    case Mul(a, b)  => sim(a.tree) * sim(b.tree)
    case Div(a, b)  => sim(a.tree) / sim(b.tree)
    case Mod(a, b)  => sim(a.tree) % sim(b.tree)

  def simBitwiseOp(e: BitwiseOpExpression[?]): Any = e match
    case BitwiseAnd(a, b)  => simScalar(a) & simScalar(b)
    case BitwiseOr(a, b)   => simScalar(a) | simScalar(b)
    case BitwiseXor(a, b)  => simScalar(a) ^ simScalar(b)
    case BitwiseNot(a)     => ~simScalar(a)
    case ShiftLeft(a, by)  => simScalar(a) << simUInt(by)
    case ShiftRight(a, by) => simScalar(a) >> simUInt(by)

  def simCompareOp(e: ComparisonOpExpression[?]): Any = e match
    case GreaterThan(a, b)      => simScalar(a) > simScalar(b)
    case LessThan(a, b)         => simScalar(a) < simScalar(b)
    case GreaterThanEqual(a, b) => simScalar(a) >= simScalar(b)
    case LessThanEqual(a, b)    => simScalar(a) <= simScalar(b)
    case Equal(a, b)            => simScalar(a) == simScalar(b)

  def simConvert(e: ConvertExpression[?, ?]): Any = e match
    case ToFloat32(a) => simScalar(a)
    case ToInt32(a)   => simScalar(a)
    case ToUInt32(a)  => simScalar(a)

  def simConst(e: Const[?]): Float | Int | Boolean = e match
    case ConstFloat32(value) => value
    case ConstInt32(value)   => value
    case ConstUInt32(value)  => value
    case ConstGB(value)      => value

  def simValue(v: Value): Any = v match
    case v: Scalar => simScalar(v)
    case v: Vec[?] => simVector(v)

  def simScalar(v: Scalar): Any = v match
    case v: FloatType     => simFloat(v)
    case v: IntType       => simInt(v)
    case v: UIntType      => simUInt(v)
    case GBoolean(source) => ???

  def simFloat(v: FloatType): Float = v match
    case Float32(tree) => sim(tree)
    case _             => throw IllegalArgumentException("wrong argument, should be FloatType")

  def simInt(v: IntType): Int = v match
    case Int32(tree) => sim(tree)
    case _           => throw IllegalArgumentException("wrong argument, should be IntType")

  def simUInt(v: UIntType): Int = v match
    case UInt32(tree) => sim(tree)
    case _            => throw IllegalArgumentException("wrong argument, should be UIntType")

  def simVector(v: Vec[?]): Any = v match
    case Vec2(tree) => ???
    case Vec3(tree) => ???
    case Vec4(tree) => ???
