package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import macros.FnCall.FnIdentifier
import control.Scope
import izumi.reflect.Tag

object Simulate:
  type Result = Float | Int | Boolean | Vector[?]

  def sim(e: Expression[?]): Result = e match
    case e: PhantomExpression[?]      => ???
    case Negate(a)                    => -simValue(a).asInstanceOf[Float]
    case e: BinaryOpExpression[?]     => simBinOp(e)
    case ScalarProd(a, b)             => scale(simVector(a), simScalar(b).asInstanceOf[Float])
    case DotProd(a, b)                => dot(simVector(a), simVector(b))
    case e: BitwiseOpExpression[?]    => simBitwiseOp(e)
    case e: ComparisonOpExpression[?] => simCompareOp(e)
    case And(a, b)                    => simScalar(a).asInstanceOf[Boolean] && simScalar(b).asInstanceOf[Boolean]
    case Or(a, b)                     => simScalar(a).asInstanceOf[Boolean] || simScalar(b).asInstanceOf[Boolean]
    case Not(a)                       => !simScalar(a).asInstanceOf[Boolean]
    case ExtractScalar(a, i)          => ???
    case e: ConvertExpression[?, ?]   => simConvert(e)
    case e: Const[?]                  => simConst(e)
    case ComposeVec2(a, b)            => Vector(simScalar(a), simScalar(b))
    case ComposeVec3(a, b, c)         => Vector(simScalar(a), simScalar(b), simScalar(c))
    case ComposeVec4(a, b, c, d)      => Vector(simScalar(a), simScalar(b), simScalar(c), simScalar(d))
    case ExtFunctionCall(fn, args)    => simExtFunc(fn, args.map(simValue))
    case FunctionCall(fn, body, args) => simFunc(fn, simScope(body), args.map(simValue))
    case InvocationId                 => ???
    case Pass(value)                  => simValue(value)
    case Dynamic(source)              => ???
    case _                            => throw IllegalArgumentException("wrong argument")

  def simBinOp(e: BinaryOpExpression[?]): Result = e match
    case Sum(a, b)  => sim(a.tree).asInstanceOf[Int] + sim(b.tree).asInstanceOf[Int]
    case Diff(a, b) => sim(a.tree).asInstanceOf[Int] - sim(b.tree).asInstanceOf[Int]
    case Mul(a, b)  => sim(a.tree).asInstanceOf[Int] * sim(b.tree).asInstanceOf[Int]
    case Div(a, b)  => sim(a.tree).asInstanceOf[Int] / sim(b.tree).asInstanceOf[Int]
    case Mod(a, b)  => sim(a.tree).asInstanceOf[Int] % sim(b.tree).asInstanceOf[Int]

  def simBitwiseOp(e: BitwiseOpExpression[?]): Result = e match
    case BitwiseAnd(a, b)  => simScalar(a).asInstanceOf[Int] & simScalar(b).asInstanceOf[Int]
    case BitwiseOr(a, b)   => simScalar(a).asInstanceOf[Int] | simScalar(b).asInstanceOf[Int]
    case BitwiseXor(a, b)  => simScalar(a).asInstanceOf[Int] ^ simScalar(b).asInstanceOf[Int]
    case BitwiseNot(a)     => ~simScalar(a).asInstanceOf[Int]
    case ShiftLeft(a, by)  => simScalar(a).asInstanceOf[Int] << simUInt(by).asInstanceOf[Int]
    case ShiftRight(a, by) => simScalar(a).asInstanceOf[Int] >> simUInt(by).asInstanceOf[Int]

  def simCompareOp(e: ComparisonOpExpression[?]): Boolean = e match
    case GreaterThan(a, b)      => simScalar(a).asInstanceOf[Float] > simScalar(b).asInstanceOf[Float]
    case LessThan(a, b)         => simScalar(a).asInstanceOf[Float] < simScalar(b).asInstanceOf[Float]
    case GreaterThanEqual(a, b) => simScalar(a).asInstanceOf[Float] >= simScalar(b).asInstanceOf[Float]
    case LessThanEqual(a, b)    => simScalar(a).asInstanceOf[Float] <= simScalar(b).asInstanceOf[Float]
    case Equal(a, b)            => simScalar(a).asInstanceOf[Float] == simScalar(b).asInstanceOf[Float]

  def simConvert(e: ConvertExpression[?, ?]): Float | Int = e match
    case ToFloat32(a) => simScalar(a).asInstanceOf[Float]
    case ToInt32(a)   => simScalar(a).asInstanceOf[Int]
    case ToUInt32(a)  => simScalar(a).asInstanceOf[Int]

  def simConst(e: Const[?]): Float | Int | Boolean = e match
    case ConstFloat32(value) => value
    case ConstInt32(value)   => value
    case ConstUInt32(value)  => value
    case ConstGB(value)      => value

  def simValue(v: Value): Result = v match
    case v: Scalar => simScalar(v)
    case v: Vec[?] => simVector(v)

  def simScalar(v: Scalar): Float | Int | Boolean = v match
    case v: FloatType     => simFloat(v)
    case v: IntType       => simInt(v).asInstanceOf[Int]
    case v: UIntType      => simUInt(v).asInstanceOf[Int]
    case GBoolean(source) => sim(source).asInstanceOf[Boolean]

  def simFloat(v: FloatType): Float = v match
    case Float32(tree) => sim(tree).asInstanceOf[Float]
    case _             => throw IllegalArgumentException("wrong argument, should be FloatType")

  def simInt(v: IntType): Int = v match
    case Int32(tree) => sim(tree).asInstanceOf[Int]
    case _           => throw IllegalArgumentException("wrong argument, should be IntType")

  def simUInt(v: UIntType): Int = v match
    case UInt32(tree) => sim(tree).asInstanceOf[Int]
    case _            => throw IllegalArgumentException("wrong argument, should be UIntType")

  def simVector(v: Vec[?]): Vector[Float | Int | Boolean] = v match
    case Vec2(tree) => Vector(???)
    case Vec3(tree) => Vector(???)
    case Vec4(tree) => Vector(???)

  def dot(v: Vector[?], w: Vector[?]) = v
    .zip(w)
    .map: (x, y) =>
      x.asInstanceOf[Float] * y.asInstanceOf[Float]
    .sum

  def scale(v: Vector[?], s: Float): Vector[?] = v.map(x => x.asInstanceOf[Float] * s)

  def simExtFunc(fn: FunctionName, args: List[Result]): Result = ???
  def simFunc(fn: FnIdentifier, body: Result, args: List[Result]): Float | Int | Boolean | Vector[?] = ???
  def simScope(body: Scope[?]) = sim(body.expr)
