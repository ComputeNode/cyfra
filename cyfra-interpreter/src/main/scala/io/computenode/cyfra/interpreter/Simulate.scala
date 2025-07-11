package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, macros.FnCall.FnIdentifier, control.Scope
import collections.*, GArray.GArrayElem, GSeq.{CurrentElem, AggregateElem, FoldSeq}
import struct.*, GStruct.{ComposeStruct, GetField}
import io.computenode.cyfra.spirv.BlockBuilder.buildBlock
import collection.mutable.Map as MMap

object Simulate:
  import Result.*

  def sim(v: Value): Result = sim(v.tree) // helpful wrapper for Value instead of Expression

  def sim(e: Expression[?]): Result =
    val exprMap = MMap.empty[Int, Result] // treeid of Expr -> result of evaluating that Expr
    val blocks = buildBlock(e)
    simIterate(blocks)(using exprMap)

  @annotation.tailrec
  def simIterate(blocks: List[Expression[?]])(using exprMap: MMap[Int, Result]): Result = blocks match
    case head :: Nil  => simOne(head)
    case head :: next =>
      val result = simOne(head)
      exprMap.addOne(head.treeid -> result)
      simIterate(next)
    case Nil => ??? // should not happen

  def simOne(e: Expression[?])(using exprMap: MMap[Int, Result]): Result = e match
    case e: PhantomExpression[?]      => simPhantom(e)
    case Negate(a)                    => simValue(a).negate
    case e: BinaryOpExpression[?]     => simBinOp(e)
    case ScalarProd(a, b)             => simVector(a).scale(simScalar(b))
    case DotProd(a, b)                => simVector(a).dot(simVector(b))
    case e: BitwiseOpExpression[?]    => simBitwiseOp(e)
    case e: ComparisonOpExpression[?] => simCompareOp(e)
    case And(a, b)                    => simScalar(a) && simScalar(b)
    case Or(a, b)                     => simScalar(a) || simScalar(b)
    case Not(a)                       => simScalar(a).negate
    case ExtractScalar(a, i)          => ??? // simVector(a), simConst(i.tree)
    case e: ConvertExpression[?, ?]   => simConvert(e)
    case e: Const[?]                  => simConst(e)
    case ComposeVec2(a, b)            => Vector(simScalar(a), simScalar(b))
    case ComposeVec3(a, b, c)         => Vector(simScalar(a), simScalar(b), simScalar(c))
    case ComposeVec4(a, b, c, d)      => Vector(simScalar(a), simScalar(b), simScalar(c), simScalar(d))
    case ExtFunctionCall(fn, args)    => ??? // simExtFunc(fn, args.map(simValue))
    case FunctionCall(fn, body, args) => ??? // simFunc(fn, simScope(body), args.map(simValue))
    case InvocationId                 => ???
    case Pass(value)                  => ???
    case Dynamic(source)              => ???
    case e: WhenExpr[?]               => simWhen(e)
    case e: ReadBuffer[?]             => simReadBuffer(e)
    case e: ReadUniform[?]            => simReadUniform(e)
    case e: GArrayElem[?]             => simGArrayElem(e)
    case e: FoldSeq[?, ?]             => simFoldSeq(e)
    case e: ComposeStruct[?]          => simComposeStruct(e)
    case e: GetField[?, ?]            => simGetField(e)
    case _                            => throw IllegalArgumentException("sim: wrong argument")

  private def simPhantom(e: PhantomExpression[?])(using exprMap: MMap[Int, Result]): Result = e match
    case CurrentElem(tid: Int)   => ???
    case AggregateElem(tid: Int) => ???

  private def simBinOp(e: BinaryOpExpression[?])(using exprMap: MMap[Int, Result]): Result = e match
    case Sum(a, b)  => simValue(a).add(simValue(b)) // scalar or vector
    case Diff(a, b) => simValue(a).sub(simValue(b)) // scalar or vector
    case Mul(a, b)  => simScalar(a).mul(simScalar(b))
    case Div(a, b)  => simScalar(a).div(simScalar(b))
    case Mod(a, b)  => simScalar(a).mod(simScalar(b))

  private def simBitwiseOp(e: BitwiseOpExpression[?])(using exprMap: MMap[Int, Result]): Int = e match
    case e: BitwiseBinaryOpExpression[?] => simBitwiseBinOp(e)
    case BitwiseNot(a)                   => simScalar(a).bitNeg
    case ShiftLeft(a, by)                => simScalar(a).shiftLeft(simScalar(by))
    case ShiftRight(a, by)               => simScalar(a).shiftRight(simScalar(by))

  private def simBitwiseBinOp(e: BitwiseBinaryOpExpression[?])(using exprMap: MMap[Int, Result]): Int = e match
    case BitwiseAnd(a, b) => simScalar(a).bitAnd(simScalar(b))
    case BitwiseOr(a, b)  => simScalar(a).bitOr(simScalar(b))
    case BitwiseXor(a, b) => simScalar(a).bitXor(simScalar(b))

  private def simCompareOp(e: ComparisonOpExpression[?])(using exprMap: MMap[Int, Result]): Boolean = e match
    case GreaterThan(a, b)      => simScalar(a).gt(simScalar(b))
    case LessThan(a, b)         => simScalar(a).lt(simScalar(b))
    case GreaterThanEqual(a, b) => simScalar(a).gteq(simScalar(b))
    case LessThanEqual(a, b)    => simScalar(a).lteq(simScalar(b))
    case Equal(a, b)            => simScalar(a).eql(simScalar(b))

  private def simConvert(e: ConvertExpression[?, ?])(using exprMap: MMap[Int, Result]): Float | Int = e match
    case ToFloat32(a) =>
      exprMap(a.treeid) match
        case f: Float => f
        case _        => throw IllegalArgumentException("ToFloat32: wrong argument type")
    case ToInt32(a) =>
      exprMap(a.treeid) match
        case n: Int => n
        case _      => throw IllegalArgumentException("ToInt32: wrong argument type")
    case ToUInt32(a) =>
      exprMap(a.treeid) match
        case n: Int => n
        case _      => throw IllegalArgumentException("ToUInt32: wrong argument type")

  private def simConst(e: Const[?])(using exprMap: MMap[Int, Result]): ScalarRes = e match
    case ConstFloat32(value) => value
    case ConstInt32(value)   => value
    case ConstUInt32(value)  => value
    case ConstGB(value)      => value

  private def simValue(v: Value)(using exprMap: MMap[Int, Result]): Result = v match
    case v: Scalar => simScalar(v)
    case v: Vec[?] => simVector(v)

  private def simScalar(v: Scalar)(using exprMap: MMap[Int, Result]): ScalarRes = v match
    case v: FloatType     => exprMap(v.tree.treeid).asInstanceOf[Float]
    case v: IntType       => exprMap(v.tree.treeid).asInstanceOf[Int]
    case v: UIntType      => exprMap(v.tree.treeid).asInstanceOf[Int]
    case GBoolean(source) => exprMap(source.treeid).asInstanceOf[Boolean]

  private def simVector(v: Vec[?])(using exprMap: MMap[Int, Result]) = v match
    case Vec2(tree) => exprMap(v.tree.treeid)
    case Vec3(tree) => exprMap(v.tree.treeid)
    case Vec4(tree) => exprMap(v.tree.treeid)

  private def simExtFunc(fn: FunctionName, args: List[Result])(using exprMap: MMap[Int, Result]): Result = ???
  private def simFunc(fn: FnIdentifier, body: Result, args: List[Result])(using exprMap: MMap[Int, Result]): Result = ???
  private def simScope(body: Scope[?])(using exprMap: MMap[Int, Result]) = exprMap(body.expr.treeid)

  @annotation.tailrec
  private def whenHelper(
    when: Expression[GBoolean],
    thenCode: Scope[?],
    otherConds: List[Scope[GBoolean]],
    otherCaseCodes: List[Scope[?]],
    otherwise: Scope[?],
  )(using exprMap: MMap[Int, Result]): Result =
    if exprMap(when.treeid).asInstanceOf[Boolean] then sim(thenCode.expr)
    else
      otherConds.headOption match
        case None       => exprMap(otherwise.expr.treeid)
        case Some(cond) =>
          whenHelper(
            when = cond.expr,
            thenCode = otherCaseCodes.head,
            otherConds = otherConds.tail,
            otherCaseCodes = otherCaseCodes.tail,
            otherwise = otherwise,
          )

  private def simWhen(e: WhenExpr[?])(using exprMap: MMap[Int, Result]): Result = e match
    case WhenExpr(when, thenCode, otherConds, otherCaseCodes, otherwise) =>
      whenHelper(when.tree, thenCode, otherConds, otherCaseCodes, otherwise)

  private def simReadBuffer(buf: ReadBuffer[?])(using exprMap: MMap[Int, Result]): Result = buf match
    case ReadBuffer(buffer, index) => ???

  private def simReadUniform(uni: ReadUniform[?])(using exprMap: MMap[Int, Result]): Result = uni match
    case ReadUniform(uniform) => ???

  private def simGArrayElem(gElem: GArrayElem[?])(using exprMap: MMap[Int, Result]): Result = gElem match
    case GArrayElem(index, i) => ???

  private def simFoldSeq(seq: FoldSeq[?, ?])(using exprMap: MMap[Int, Result]): Result = seq match
    case FoldSeq(zero, fn, seq) => ???

  private def simComposeStruct(cs: ComposeStruct[?])(using exprMap: MMap[Int, Result]): Result = cs match
    case ComposeStruct(fields, resultSchema) => ???

  private def simGetField(gf: GetField[?, ?])(using exprMap: MMap[Int, Result]): Result = gf match
    case GetField(struct, fieldIndex) => ???
