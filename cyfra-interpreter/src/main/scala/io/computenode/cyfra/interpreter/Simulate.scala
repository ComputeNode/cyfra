package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, macros.FnCall.FnIdentifier, control.Scope
import collections.*, GArray.GArrayElem, GSeq.{CurrentElem, AggregateElem, FoldSeq}
import struct.*, GStruct.{ComposeStruct, GetField}
import io.computenode.cyfra.spirv.BlockBuilder.buildBlock
import collection.mutable.Map as MMap

object Simulate:
  import Result.*

  def sim(v: Value, sc: SimContext): (Result, SimContext) = sim(v.tree, sc)
  def sim(e: Expression[?], sc: SimContext = SimContext()): (Result, SimContext) = simIterate(buildBlock(e), sc)(using Map())

  @annotation.tailrec
  def simIterate(blocks: List[Expression[?]], sc: SimContext)(using exprMap: Map[Int, Result]): (Result, SimContext) = blocks match
    case head :: Nil  => simOne(head, sc)
    case head :: next =>
      val (result, newSc) = simOne(head, sc) // context updated if there are reads/writes
      val newExprMap = exprMap + (head.treeid -> result) // update map with new result
      simIterate(next, newSc)(using newExprMap)
    case Nil => ??? // should not happen

  def simOne(e: Expression[?], sc: SimContext)(using exprMap: Map[Int, Result]): (Result, SimContext) = e match
    case e: PhantomExpression[?]      => (simPhantom(e), sc)
    case Negate(a)                    => (simValue(a).negate, sc)
    case e: BinaryOpExpression[?]     => (simBinOp(e), sc)
    case ScalarProd(a, b)             => (simVector(a).scale(simScalar(b)), sc)
    case DotProd(a, b)                => (simVector(a).dot(simVector(b)), sc)
    case e: BitwiseOpExpression[?]    => (simBitwiseOp(e), sc)
    case e: ComparisonOpExpression[?] => (simCompareOp(e), sc)
    case And(a, b)                    => (simScalar(a) && simScalar(b), sc)
    case Or(a, b)                     => (simScalar(a) || simScalar(b), sc)
    case Not(a)                       => (simScalar(a).negate, sc)
    case ExtractScalar(a, i)          => (simVector(a).apply(simValue(i).asInstanceOf[Int]), sc)
    case e: ConvertExpression[?, ?]   => (simConvert(e), sc)
    case e: Const[?]                  => (simConst(e), sc)
    case ComposeVec2(a, b)            => (Vector(simScalar(a), simScalar(b)), sc)
    case ComposeVec3(a, b, c)         => (Vector(simScalar(a), simScalar(b), simScalar(c)), sc)
    case ComposeVec4(a, b, c, d)      => (Vector(simScalar(a), simScalar(b), simScalar(c), simScalar(d)), sc)
    case ExtFunctionCall(fn, args)    => ??? // simExtFunc(fn, args.map(simValue), sc)
    case FunctionCall(fn, body, args) => ??? // simFunc(fn, simScope(body), args.map(simValue), sc)
    case InvocationId                 => ???
    case Pass(value)                  => ???
    case Dynamic(source)              => ???
    case e: WhenExpr[?]               => simWhen(e, sc) // returns new SimContext
    case e: ReadBuffer[?]             => simReadBuffer(e, sc) // returns new SimContext
    case e: ReadUniform[?]            => simReadUniform(e)
    case e: GArrayElem[?]             => simGArrayElem(e)
    case e: FoldSeq[?, ?]             => simFoldSeq(e)
    case e: ComposeStruct[?]          => simComposeStruct(e)
    case e: GetField[?, ?]            => simGetField(e)
    case _                            => throw IllegalArgumentException("sim: wrong argument")

  private def simPhantom(e: PhantomExpression[?]): Result = e match
    case CurrentElem(tid: Int)   => ???
    case AggregateElem(tid: Int) => ???

  private def simBinOp(e: BinaryOpExpression[?])(using exprMap: Map[Int, Result]): Result = e match
    case Sum(a, b)  => simValue(a).add(simValue(b)) // scalar or vector
    case Diff(a, b) => simValue(a).sub(simValue(b)) // scalar or vector
    case Mul(a, b)  => simScalar(a).mul(simScalar(b))
    case Div(a, b)  => simScalar(a).div(simScalar(b))
    case Mod(a, b)  => simScalar(a).mod(simScalar(b))

  private def simBitwiseOp(e: BitwiseOpExpression[?])(using exprMap: Map[Int, Result]): Int = e match
    case e: BitwiseBinaryOpExpression[?] => simBitwiseBinOp(e)
    case BitwiseNot(a)                   => simScalar(a).bitNeg
    case ShiftLeft(a, by)                => simScalar(a).shiftLeft(simScalar(by))
    case ShiftRight(a, by)               => simScalar(a).shiftRight(simScalar(by))

  private def simBitwiseBinOp(e: BitwiseBinaryOpExpression[?])(using exprMap: Map[Int, Result]): Int = e match
    case BitwiseAnd(a, b) => simScalar(a).bitAnd(simScalar(b))
    case BitwiseOr(a, b)  => simScalar(a).bitOr(simScalar(b))
    case BitwiseXor(a, b) => simScalar(a).bitXor(simScalar(b))

  private def simCompareOp(e: ComparisonOpExpression[?])(using exprMap: Map[Int, Result]): Boolean = e match
    case GreaterThan(a, b)      => simScalar(a).gt(simScalar(b))
    case LessThan(a, b)         => simScalar(a).lt(simScalar(b))
    case GreaterThanEqual(a, b) => simScalar(a).gteq(simScalar(b))
    case LessThanEqual(a, b)    => simScalar(a).lteq(simScalar(b))
    case Equal(a, b)            => simScalar(a).eql(simScalar(b))

  private def simConvert(e: ConvertExpression[?, ?])(using exprMap: Map[Int, Result]): Float | Int = e match
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

  private def simConst(e: Const[?]): ScalarRes = e match
    case ConstFloat32(value) => value
    case ConstInt32(value)   => value
    case ConstUInt32(value)  => value
    case ConstGB(value)      => value

  private def simValue(v: Value)(using exprMap: Map[Int, Result]): Result = v match
    case v: Scalar => simScalar(v)
    case v: Vec[?] => simVector(v)

  private def simScalar(v: Scalar)(using exprMap: Map[Int, Result]): ScalarRes = v match
    case v: FloatType     => exprMap(v.tree.treeid).asInstanceOf[Float]
    case v: IntType       => exprMap(v.tree.treeid).asInstanceOf[Int]
    case v: UIntType      => exprMap(v.tree.treeid).asInstanceOf[Int]
    case GBoolean(source) => exprMap(source.treeid).asInstanceOf[Boolean]

  private def simVector(v: Vec[?])(using exprMap: Map[Int, Result]): Vector[ScalarRes] = v match
    case Vec2(tree) => exprMap(tree.treeid).asInstanceOf[Vector[ScalarRes]]
    case Vec3(tree) => exprMap(tree.treeid).asInstanceOf[Vector[ScalarRes]]
    case Vec4(tree) => exprMap(tree.treeid).asInstanceOf[Vector[ScalarRes]]

  private def simExtFunc(fn: FunctionName, args: List[Result], sc: SimContext): (Result, SimContext) = ???
  private def simFunc(fn: FnIdentifier, body: Result, args: List[Result], sc: SimContext): (Result, SimContext) = ???

  @annotation.tailrec
  private def whenHelper(
    when: Expression[GBoolean],
    thenCode: Scope[?],
    otherConds: List[Scope[GBoolean]],
    otherCaseCodes: List[Scope[?]],
    otherwise: Scope[?],
    sc: SimContext,
  ): (Result, SimContext) =
    // scopes are not included in the "main" exprMap, they have to be simulated from scratch.
    // there could be reads/writes happening in scopes, SimContext has to be updated.
    val (boolRes, newSc) = sim(when, sc)
    if boolRes.asInstanceOf[Boolean] then sim(thenCode.expr, newSc)
    else
      otherConds.headOption match
        case None       => sim(otherwise.expr, newSc)
        case Some(cond) =>
          whenHelper(
            when = cond.expr,
            thenCode = otherCaseCodes.head,
            otherConds = otherConds.tail,
            otherCaseCodes = otherCaseCodes.tail,
            otherwise = otherwise,
            sc = newSc,
          )

  private def simWhen(e: WhenExpr[?], sc: SimContext): (Result, SimContext) = e match
    case WhenExpr(when, thenCode, otherConds, otherCaseCodes, otherwise) =>
      whenHelper(when.tree, thenCode, otherConds, otherCaseCodes, otherwise, sc)

  private def simReadBuffer(buf: ReadBuffer[?], sc: SimContext)(using exprMap: Map[Int, Result]): (Result, SimContext) = buf match
    case ReadBuffer(buffer, index) =>
      val i = exprMap(index.tree.treeid).asInstanceOf[Int]
      val newSc = sc.addRead(ReadBuf(buffer, i))
      (newSc.lookup(buffer, i), newSc)

  private def simReadUniform(uni: ReadUniform[?]): (Result, SimContext) = uni match
    case ReadUniform(uniform) => ???

  private def simGArrayElem(gElem: GArrayElem[?]): (Result, SimContext) = gElem match
    case GArrayElem(index, i) => ???

  private def simFoldSeq(seq: FoldSeq[?, ?]): (Result, SimContext) = seq match
    case FoldSeq(zero, fn, seq) => ???

  private def simComposeStruct(cs: ComposeStruct[?]): (Result, SimContext) = cs match
    case ComposeStruct(fields, resultSchema) => ???

  private def simGetField(gf: GetField[?, ?]): (Result, SimContext) = gf match
    case GetField(struct, fieldIndex) => ???
