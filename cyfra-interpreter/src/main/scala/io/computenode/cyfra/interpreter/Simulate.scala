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
    case ScalarProd(a, b)             => simVector(a) scale simScalar(b)
    case DotProd(a, b)                => simVector(a) dot simVector(b)
    case e: BitwiseOpExpression[?]    => simBitwiseOp(e)
    case e: ComparisonOpExpression[?] => simCompareOp(e)
    case And(a, b)                    => simScalar(a) && simScalar(b)
    case Or(a, b)                     => simScalar(a) || simScalar(b)
    case Not(a)                       => simScalar(a).neg
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
    case e: WhenExpr[?]               => simWhen(e)
    case _                            => throw IllegalArgumentException("wrong argument")

  def simBinOp(e: BinaryOpExpression[?]): Result = e match
    case Sum(a, b)  => simValue(a) add simValue(b)
    case Diff(a, b) => simValue(a) sub simValue(b)
    case Mul(a, b)  => simScalar(a) mul simScalar(b)
    case Div(a, b)  => simScalar(a) div simScalar(b)
    case Mod(a, b)  => simScalar(a) mod simScalar(b)

  def simBitwiseOp(e: BitwiseOpExpression[?]): Int = e match
    case e: BitwiseBinaryOpExpression[?] => simBitwiseBinOp(e)
    case BitwiseNot(a)                   =>
      simScalar(a) match
        case m: Int => ~m
        case _      => throw IllegalArgumentException("BitwiseNot: wrong argument type")
    case ShiftLeft(a, by) =>
      (simScalar(a), simScalar(by)) match
        case (m: Int, n: Int) => m << n
        case _                => throw IllegalArgumentException("ShiftLeft: wrong argument types")
    case ShiftRight(a, by) =>
      (simScalar(a), simScalar(by)) match
        case (m: Int, n: Int) => m >> n
        case _                => throw IllegalArgumentException("ShiftRight: wrong argument types")

  def simBitwiseBinOp(e: BitwiseBinaryOpExpression[?]) = e match
    case BitwiseAnd(a, b) =>
      (simScalar(a), simScalar(b)) match
        case (m: Int, n: Int) => m & n
        case _                => throw IllegalArgumentException("BitwiseAnd: wrong argument types")
    case BitwiseOr(a, b) =>
      (simScalar(a), simScalar(b)) match
        case (m: Int, n: Int) => m | n
        case _                => throw IllegalArgumentException("BitwiseOr: wrong argument types")
    case BitwiseXor(a, b) =>
      (simScalar(a), simScalar(b)) match
        case (m: Int, n: Int) => m ^ n
        case _                => throw IllegalArgumentException("BitwiseXor: wrong argument types")

  def simCompareOp(e: ComparisonOpExpression[?]): Boolean = e match
    case GreaterThan(a, b)      => simScalar(a) > simScalar(b)
    case LessThan(a, b)         => simScalar(a) < simScalar(b)
    case GreaterThanEqual(a, b) => simScalar(a) >= simScalar(b)
    case LessThanEqual(a, b)    => simScalar(a) <= simScalar(b)
    case Equal(a, b)            => simScalar(a) eql simScalar(b)

  def simConvert(e: ConvertExpression[?, ?]): Float | Int = e match
    case ToFloat32(a) =>
      simScalar(a) match
        case f: Float => f
        case _        => throw IllegalArgumentException("ToFloat32: wrong argument type")
    case ToInt32(a) =>
      simScalar(a) match
        case n: Int => n
        case _      => throw IllegalArgumentException("ToInt32: wrong argument type")
    case ToUInt32(a) =>
      simScalar(a) match
        case n: Int => n
        case _      => throw IllegalArgumentException("ToUInt32: wrong argument type")

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

  def simExtFunc(fn: FunctionName, args: List[Result]): Result = ???
  def simFunc(fn: FnIdentifier, body: Result, args: List[Result]): Result = ???
  def simScope(body: Scope[?]) = sim(body.expr)

  @annotation.tailrec
  def whenHelper(when: GBoolean, thenCode: Scope[?], otherConds: List[Scope[GBoolean]], otherCaseCodes: List[Scope[?]], otherwise: Scope[?]): Result =
    if sim(when.tree).asInstanceOf[Boolean] then sim(thenCode.expr)
    else
      otherConds.headOption match
        case None       => sim(otherwise.expr)
        case Some(cond) =>
          whenHelper(
            when = GBoolean(cond.expr),
            thenCode = otherCaseCodes.head,
            otherConds = otherConds.tail,
            otherCaseCodes = otherCaseCodes.tail,
            otherwise = otherwise,
          )

  def simWhen(e: WhenExpr[?]): Result = e match
    case WhenExpr(when, thenCode, otherConds, otherCaseCodes, otherwise) =>
      whenHelper(when, thenCode, otherConds, otherCaseCodes, otherwise)
