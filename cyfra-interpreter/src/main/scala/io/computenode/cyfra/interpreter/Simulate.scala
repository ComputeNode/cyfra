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
  def sim(e: Expression[?])(using sc: SimContext = SimContext()): Result = simIterate(buildBlock(e))

  @annotation.tailrec
  def simIterate(blocks: List[Expression[?]])(using sc: SimContext): Result = blocks match
    case head :: Nil  => simOne(head)
    case head :: next =>
      val result = simOne(head)
      sc.addResult(head.treeid, result)
      simIterate(next)
    case Nil => ??? // should not happen

  def simOne(e: Expression[?])(using sc: SimContext): Result = e match
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
    case ExtractScalar(a, i)          => simVector(a).apply(simValue(i).asInstanceOf[Int])
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

  private def simPhantom(e: PhantomExpression[?])(using sc: SimContext): Result = e match
    case CurrentElem(tid: Int)   => ???
    case AggregateElem(tid: Int) => ???

  private def simBinOp(e: BinaryOpExpression[?])(using sc: SimContext): Result = e match
    case Sum(a, b)  => simValue(a).add(simValue(b)) // scalar or vector
    case Diff(a, b) => simValue(a).sub(simValue(b)) // scalar or vector
    case Mul(a, b)  => simScalar(a).mul(simScalar(b))
    case Div(a, b)  => simScalar(a).div(simScalar(b))
    case Mod(a, b)  => simScalar(a).mod(simScalar(b))

  private def simBitwiseOp(e: BitwiseOpExpression[?])(using sc: SimContext): Int = e match
    case e: BitwiseBinaryOpExpression[?] => simBitwiseBinOp(e)
    case BitwiseNot(a)                   => simScalar(a).bitNeg
    case ShiftLeft(a, by)                => simScalar(a).shiftLeft(simScalar(by))
    case ShiftRight(a, by)               => simScalar(a).shiftRight(simScalar(by))

  private def simBitwiseBinOp(e: BitwiseBinaryOpExpression[?])(using sc: SimContext): Int = e match
    case BitwiseAnd(a, b) => simScalar(a).bitAnd(simScalar(b))
    case BitwiseOr(a, b)  => simScalar(a).bitOr(simScalar(b))
    case BitwiseXor(a, b) => simScalar(a).bitXor(simScalar(b))

  private def simCompareOp(e: ComparisonOpExpression[?])(using sc: SimContext): Boolean = e match
    case GreaterThan(a, b)      => simScalar(a).gt(simScalar(b))
    case LessThan(a, b)         => simScalar(a).lt(simScalar(b))
    case GreaterThanEqual(a, b) => simScalar(a).gteq(simScalar(b))
    case LessThanEqual(a, b)    => simScalar(a).lteq(simScalar(b))
    case Equal(a, b)            => simScalar(a).eql(simScalar(b))

  private def simConvert(e: ConvertExpression[?, ?])(using sc: SimContext): Float | Int = e match
    case ToFloat32(a) =>
      sc.lookup(a.treeid) match
        case f: Float => f
        case _        => throw IllegalArgumentException("ToFloat32: wrong argument type")
    case ToInt32(a) =>
      sc.lookup(a.treeid) match
        case n: Int => n
        case _      => throw IllegalArgumentException("ToInt32: wrong argument type")
    case ToUInt32(a) =>
      sc.lookup(a.treeid) match
        case n: Int => n
        case _      => throw IllegalArgumentException("ToUInt32: wrong argument type")

  private def simConst(e: Const[?]): ScalarRes = e match
    case ConstFloat32(value) => value
    case ConstInt32(value)   => value
    case ConstUInt32(value)  => value
    case ConstGB(value)      => value

  private def simValue(v: Value)(using sc: SimContext): Result = v match
    case v: Scalar => simScalar(v)
    case v: Vec[?] => simVector(v)

  private def simScalar(v: Scalar)(using sc: SimContext): ScalarRes = v match
    case v: FloatType     => sc.lookup(v.tree.treeid).asInstanceOf[Float]
    case v: IntType       => sc.lookup(v.tree.treeid).asInstanceOf[Int]
    case v: UIntType      => sc.lookup(v.tree.treeid).asInstanceOf[Int]
    case GBoolean(source) => sc.lookup(source.treeid).asInstanceOf[Boolean]

  private def simVector(v: Vec[?])(using sc: SimContext) = v match
    case Vec2(tree) => sc.lookup(tree.treeid).asInstanceOf[Vector[ScalarRes]]
    case Vec3(tree) => sc.lookup(tree.treeid).asInstanceOf[Vector[ScalarRes]]
    case Vec4(tree) => sc.lookup(tree.treeid).asInstanceOf[Vector[ScalarRes]]

  private def simExtFunc(fn: FunctionName, args: List[Result])(using sc: SimContext): Result = ???
  private def simFunc(fn: FnIdentifier, body: Result, args: List[Result])(using sc: SimContext): Result = ???

  @annotation.tailrec
  private def whenHelper(
    when: Expression[GBoolean],
    thenCode: Scope[?],
    otherConds: List[Scope[GBoolean]],
    otherCaseCodes: List[Scope[?]],
    otherwise: Scope[?],
  )(using sc: SimContext): Result =
    if sim(when).asInstanceOf[Boolean] then sim(thenCode.expr)
    else
      otherConds.headOption match
        case None       => sim(otherwise.expr)
        case Some(cond) =>
          whenHelper(
            when = cond.expr,
            thenCode = otherCaseCodes.head,
            otherConds = otherConds.tail,
            otherCaseCodes = otherCaseCodes.tail,
            otherwise = otherwise,
          )

  private def simWhen(e: WhenExpr[?])(using sc: SimContext): Result = e match
    case WhenExpr(when, thenCode, otherConds, otherCaseCodes, otherwise) =>
      whenHelper(when.tree, thenCode, otherConds, otherCaseCodes, otherwise)

  private def simReadBuffer(buf: ReadBuffer[?])(using sc: SimContext): Result = buf match
    case ReadBuffer(buffer, index) =>
      val i = sim(index).asInstanceOf[Int]
      sc.addRead(buffer, i)
      sc.read(buffer, i)

  private def simReadUniform(uni: ReadUniform[?])(using sc: SimContext): Result = uni match
    case ReadUniform(uniform) => ???

  private def simGArrayElem(gElem: GArrayElem[?])(using sc: SimContext): Result = gElem match
    case GArrayElem(index, i) => ???

  private def simFoldSeq(seq: FoldSeq[?, ?])(using sc: SimContext): Result = seq match
    case FoldSeq(zero, fn, seq) => ???

  private def simComposeStruct(cs: ComposeStruct[?])(using sc: SimContext): Result = cs match
    case ComposeStruct(fields, resultSchema) => ???

  private def simGetField(gf: GetField[?, ?])(using sc: SimContext): Result = gf match
    case GetField(struct, fieldIndex) => ???
