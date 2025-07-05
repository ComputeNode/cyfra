package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import macros.FnCall.FnIdentifier
import control.Scope
import izumi.reflect.Tag

object Simulate:
  import Result.*

  def sim(v: Value): Result = sim(v.tree) // helpful wrapper for Value instead of Expression

  def sim(e: Expression[?]): Result = e match
    case e: PhantomExpression[?]      => throw IllegalArgumentException("phantom expression")
    case Negate(a)                    => simValue(a).negate
    case e: BinaryOpExpression[?]     => simBinOp(e)
    case ScalarProd(a, b)             => simVector(a).scale(simScalar(b))
    case DotProd(a, b)                => simVector(a).dot(simVector(b))
    case e: BitwiseOpExpression[?]    => simBitwiseOp(e)
    case e: ComparisonOpExpression[?] => simCompareOp(e)
    case And(a, b)                    => simScalar(a) && simScalar(b)
    case Or(a, b)                     => simScalar(a) || simScalar(b)
    case Not(a)                       => simScalar(a).negateSc
    case ExtractScalar(a, i)          => ???
    case e: ConvertExpression[?, ?]   => simConvert(e)
    case e: Const[?]                  => simConst(e)
    case ComposeVec2(a, b)            => Vector(simScalar(a), simScalar(b))
    case ComposeVec3(a, b, c)         => Vector(simScalar(a), simScalar(b), simScalar(c))
    case ComposeVec4(a, b, c, d)      => Vector(simScalar(a), simScalar(b), simScalar(c), simScalar(d))
    case ExtFunctionCall(fn, args)    => simExtFunc(fn, args.map(simValue))
    case FunctionCall(fn, body, args) => simFunc(fn, simScope(body), args.map(simValue))
    case InvocationId                 => ???
    case Pass(value)                  => ???
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
    case ShiftLeft(a, by)  => simScalar(a).asInstanceOf[Int] << simScalar(by).asInstanceOf[Int]
    case ShiftRight(a, by) => simScalar(a).asInstanceOf[Int] >> simScalar(by).asInstanceOf[Int]

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

  def simConst(e: Const[?]): ScalarRes = e match
    case ConstFloat32(value) => value
    case ConstInt32(value)   => value
    case ConstUInt32(value)  => value
    case ConstGB(value)      => value

  def simValue(v: Value): Result = v match
    case v: Scalar => simScalar(v)
    case v: Vec[?] => simVector(v)

  def simScalar(v: Scalar): ScalarRes = v match
    case v: FloatType     => sim(v.tree).asInstanceOf[Float]
    case v: IntType       => sim(v.tree).asInstanceOf[Int]
    case v: UIntType      => sim(v.tree).asInstanceOf[Int]
    case GBoolean(source) => sim(source).asInstanceOf[Boolean]

  def simVector(v: Vec[?]): Vector[ScalarRes] = v match
    case Vec2(tree) => sim(tree).asInstanceOf[Vector[ScalarRes]]
    case Vec3(tree) => sim(tree).asInstanceOf[Vector[ScalarRes]]
    case Vec4(tree) => sim(tree).asInstanceOf[Vector[ScalarRes]]

  def scale(v: Vector[ScalarRes], s: Float): Vector[ScalarRes] = v.map(x => x.asInstanceOf[Float] * s)

  def simExtFunc(fn: FunctionName, args: List[Result]): Result = ???
  def simFunc(fn: FnIdentifier, body: Result, args: List[Result]): Result = ???
  def simScope(body: Scope[?]) = sim(body.expr)
