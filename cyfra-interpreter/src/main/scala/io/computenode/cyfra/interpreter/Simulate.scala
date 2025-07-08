package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, macros.FnCall.FnIdentifier, control.Scope
import collections.*, GArray.GArrayElem, GSeq.{CurrentElem, AggregateElem, FoldSeq}
import struct.*, GStruct.{ComposeStruct, GetField}

object Simulate:
  import Result.*

  def sim(v: Value): Result = sim(v.tree) // helpful wrapper for Value instead of Expression

  def sim(e: Expression[?]): Result = e match
    case e: PhantomExpression[?]      => simPhantom(e)
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
    case e: ReadBuffer[?]             => simReadBuffer(e)
    case e: ReadUniform[?]            => simReadUniform(e)
    case e: GArrayElem[?]             => simGArrayElem(e)
    case e: FoldSeq[?, ?]             => simFoldSeq(e)
    case e: ComposeStruct[?]          => ???
    case e: GetField[?, ?]            => ???
    case _                            => throw IllegalArgumentException("wrong argument")

  private def simPhantom(e: PhantomExpression[?]): Result = e match
    case CurrentElem(tid: Int)   => ???
    case AggregateElem(tid: Int) => ???

  private def simBinOp(e: BinaryOpExpression[?]): Result = e match
    case Sum(a, b)  => simValue(a) add simValue(b) // scalar or vector
    case Diff(a, b) => simValue(a) sub simValue(b) // scalar or vector
    case Mul(a, b)  => simScalar(a) * simScalar(b)
    case Div(a, b)  => simScalar(a) / simScalar(b)
    case Mod(a, b)  => simScalar(a) % simScalar(b)

  private def simBitwiseOp(e: BitwiseOpExpression[?]): Int = e match
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

  private def simBitwiseBinOp(e: BitwiseBinaryOpExpression[?]) = e match
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

  private def simCompareOp(e: ComparisonOpExpression[?]): Boolean = e match
    case GreaterThan(a, b)      => simScalar(a) > simScalar(b)
    case LessThan(a, b)         => simScalar(a) < simScalar(b)
    case GreaterThanEqual(a, b) => simScalar(a) >= simScalar(b)
    case LessThanEqual(a, b)    => simScalar(a) <= simScalar(b)
    case Equal(a, b)            => simScalar(a) eql simScalar(b)

  private def simConvert(e: ConvertExpression[?, ?]): Float | Int = e match
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

  private def simConst(e: Const[?]): ScalarRes = e match
    case ConstFloat32(value) => value
    case ConstInt32(value)   => value
    case ConstUInt32(value)  => value
    case ConstGB(value)      => value

  private def simValue(v: Value): Result = v match
    case v: Scalar => simScalar(v)
    case v: Vec[?] => simVector(v)

  private def simScalar(v: Scalar): ScalarRes = v match
    case v: FloatType     => sim(v.tree).asInstanceOf[Float]
    case v: IntType       => sim(v.tree).asInstanceOf[Int]
    case v: UIntType      => sim(v.tree).asInstanceOf[Int]
    case GBoolean(source) => sim(source).asInstanceOf[Boolean]

  private def simVector(v: Vec[?]): Vector[ScalarRes] = v match
    case Vec2(tree) => sim(tree).asInstanceOf[Vector[ScalarRes]]
    case Vec3(tree) => sim(tree).asInstanceOf[Vector[ScalarRes]]
    case Vec4(tree) => sim(tree).asInstanceOf[Vector[ScalarRes]]

  private def simExtFunc(fn: FunctionName, args: List[Result]): Result = ???
  private def simFunc(fn: FnIdentifier, body: Result, args: List[Result]): Result = ???
  private def simScope(body: Scope[?]) = sim(body.expr)

  @annotation.tailrec
  private def whenHelper(
    when: GBoolean,
    thenCode: Scope[?],
    otherConds: List[Scope[GBoolean]],
    otherCaseCodes: List[Scope[?]],
    otherwise: Scope[?],
  ): Result =
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

  private def simWhen(e: WhenExpr[?]): Result = e match
    case WhenExpr(when, thenCode, otherConds, otherCaseCodes, otherwise) =>
      whenHelper(when, thenCode, otherConds, otherCaseCodes, otherwise)

  private def simReadBuffer(buf: ReadBuffer[?]): Result = buf match
    case ReadBuffer(buffer, index) => ???

  private def simReadUniform(uni: ReadUniform[?]): Result = uni match
    case ReadUniform(uniform) => ???

  private def simGArrayElem(gElem: GArrayElem[?]): Result = gElem match
    case GArrayElem(index, i) => ???

  private def simFoldSeq(seq: FoldSeq[?, ?]): Result = seq match
    case FoldSeq(zero, fn, seq) => ???

  private def simComposeStruct(cs: ComposeStruct[?]): Result = cs match
    case ComposeStruct(fields, resultSchema) => ???

  private def simGetField(gf: GetField[?, ?]): Result = gf match
    case GetField(struct, fieldIndex) => ???
