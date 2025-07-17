package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, macros.FnCall.FnIdentifier, control.Scope
import collections.*, GArray.GArrayElem, GSeq.{CurrentElem, AggregateElem, FoldSeq}
import struct.*, GStruct.{ComposeStruct, GetField}
import io.computenode.cyfra.spirv.BlockBuilder.buildBlock
import collection.mutable.Map as MMap

object Simulate:
  import Result.*

  def sim(v: Value): (Result, SimContext) = sim(v.tree) // helpful wrapper for Value instead of Expression
  def sim(e: Expression[?], sc: SimContext = SimContext()): (Result, SimContext) = simIterate(buildBlock(e), sc)

  @annotation.tailrec
  def simIterate(blocks: List[Expression[?]], sc: SimContext): (Result, SimContext) = blocks match
    case head :: Nil  => simOne(head, sc)
    case head :: next =>
      val (result, sc1) = simOne(head, sc)
      val newSc = sc1.addResult(head.treeid, result)
      simIterate(next, newSc)
    case Nil => ??? // should not happen

  def simOne(e: Expression[?], sc: SimContext): (Result, SimContext) = e match
    case e: PhantomExpression[?] => simPhantom(e, sc)
    case Negate(a)               =>
      val (res, newSc) = simValue(a, sc)
      (res.negate, newSc)
    case e: BinaryOpExpression[?] => simBinOp(e, sc)
    case ScalarProd(a, b)         =>
      val (resA, scA) = simVector(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.scale(resB), scB)
    case DotProd(a, b) =>
      val (resA, scA) = simVector(a, sc)
      val (resB, scB) = simVector(b, scA)
      (resA.dot(resB), scB)
    case e: BitwiseOpExpression[?]    => simBitwiseOp(e, sc)
    case e: ComparisonOpExpression[?] => simCompareOp(e, sc)
    case And(a, b)                    =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA && resB, scB)
    case Or(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA || resB, scB)
    case Not(a) =>
      val (res, newSc) = simScalar(a, sc)
      (res.negate, newSc)
    case ExtractScalar(a, i) =>
      val (resA, scA) = simVector(a, sc)
      val (index, scI) = simValue(i, scA)
      (resA.apply(index.asInstanceOf[Int]), scI)
    case e: ConvertExpression[?, ?] => simConvert(e, sc)
    case e: Const[?]                => (simConst(e), sc)
    case ComposeVec2(a, b)          =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (Vector(resA, resB), scB)
    case ComposeVec3(a, b, c) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      val (resC, scC) = simScalar(c, scB)
      (Vector(resA, resB, resC), scC)
    case ComposeVec4(a, b, c, d) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      val (resC, scC) = simScalar(c, scB)
      val (resD, scD) = simScalar(d, scC)
      (Vector(resA, resB, resC, resD), scD)
    case ExtFunctionCall(fn, args)    => ??? // simExtFunc(fn, args.map(simValue), sc)
    case FunctionCall(fn, body, args) => ??? // simFunc(fn, simScope(body), args.map(simValue), sc)
    case InvocationId                 => ???
    case Pass(value)                  => ???
    case Dynamic(source)              => ???
    case e: WhenExpr[?]               => simWhen(e, sc)
    case e: ReadBuffer[?]             => simReadBuffer(e, sc)
    case e: ReadUniform[?]            => simReadUniform(e, sc)
    case e: GArrayElem[?]             => simGArrayElem(e, sc)
    case e: FoldSeq[?, ?]             => simFoldSeq(e, sc)
    case e: ComposeStruct[?]          => simComposeStruct(e, sc)
    case e: GetField[?, ?]            => simGetField(e, sc)
    case _                            => throw IllegalArgumentException("sim: wrong argument")

  private def simPhantom(e: PhantomExpression[?], sc: SimContext): (Result, SimContext) = e match
    case CurrentElem(tid: Int)   => ???
    case AggregateElem(tid: Int) => ???

  private def simBinOp(e: BinaryOpExpression[?], sc: SimContext): (Result, SimContext) = e match
    case Sum(a, b) => // scalar or vector
      val (resA, scA) = simValue(a, sc)
      val (resB, scB) = simValue(b, scA)
      (resA.add(resB), scB)
    case Diff(a, b) => // scalar or vector
      val (resA, scA) = simValue(a, sc)
      val (resB, scB) = simValue(b, scA)
      (resA.sub(resB), scB)
    case Mul(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.mul(resB), scB)
    case Div(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.div(resB), scB)
    case Mod(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.mod(resB), scB)

  private def simBitwiseOp(e: BitwiseOpExpression[?], sc: SimContext): (Int, SimContext) = e match
    case e: BitwiseBinaryOpExpression[?] => simBitwiseBinOp(e, sc)
    case BitwiseNot(a)                   =>
      val (res, newSc) = simScalar(a, sc)
      (res.bitNeg, newSc)
    case ShiftLeft(a, by) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(by, scA)
      (resA.shiftLeft(resB), scB)
    case ShiftRight(a, by) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(by, scA)
      (resA.shiftRight(resB), scB)

  private def simBitwiseBinOp(e: BitwiseBinaryOpExpression[?], sc: SimContext): (Int, SimContext) = e match
    case BitwiseAnd(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.bitAnd(resB), scB)
    case BitwiseOr(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.bitOr(resB), scB)
    case BitwiseXor(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.bitXor(resB), scB)

  private def simCompareOp(e: ComparisonOpExpression[?], sc: SimContext): (Boolean, SimContext) = e match
    case GreaterThan(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.gt(resB), scB)
    case LessThan(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.lt(resB), scB)
    case GreaterThanEqual(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.gteq(resB), scB)
    case LessThanEqual(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.lteq(resB), scB)
    case Equal(a, b) =>
      val (resA, scA) = simScalar(a, sc)
      val (resB, scB) = simScalar(b, scA)
      (resA.eql(resB), scB)

  private def simConvert(e: ConvertExpression[?, ?], sc: SimContext): (Float | Int, SimContext) = e match
    case ToFloat32(a) =>
      sc.lookupExpr(a.treeid) match
        case f: Float => (f, sc)
        case _        => throw IllegalArgumentException("ToFloat32: wrong argument type")
    case ToInt32(a) =>
      sc.lookupExpr(a.treeid) match
        case n: Int => (n, sc)
        case _      => throw IllegalArgumentException("ToInt32: wrong argument type")
    case ToUInt32(a) =>
      sc.lookupExpr(a.treeid) match
        case n: Int => (n, sc)
        case _      => throw IllegalArgumentException("ToUInt32: wrong argument type")

  private def simConst(e: Const[?]): ScalarRes = e match // no context needed
    case ConstFloat32(value) => value
    case ConstInt32(value)   => value
    case ConstUInt32(value)  => value
    case ConstGB(value)      => value

  private def simValue(v: Value, sc: SimContext): (Result, SimContext) = v match
    case v: Scalar => simScalar(v, sc)
    case v: Vec[?] => simVector(v, sc)

  private def simScalar(v: Scalar, sc: SimContext): (ScalarRes, SimContext) = v match
    case v: FloatType     => (sc.lookupExpr(v.tree.treeid).asInstanceOf[Float], sc)
    case v: IntType       => (sc.lookupExpr(v.tree.treeid).asInstanceOf[Int], sc)
    case v: UIntType      => (sc.lookupExpr(v.tree.treeid).asInstanceOf[Int], sc)
    case GBoolean(source) => (sc.lookupExpr(source.treeid).asInstanceOf[Boolean], sc)

  private def simVector(v: Vec[?], sc: SimContext): (Vector[ScalarRes], SimContext) = v match
    case Vec2(tree) => (sc.lookupExpr(tree.treeid).asInstanceOf[Vector[ScalarRes]], sc)
    case Vec3(tree) => (sc.lookupExpr(tree.treeid).asInstanceOf[Vector[ScalarRes]], sc)
    case Vec4(tree) => (sc.lookupExpr(tree.treeid).asInstanceOf[Vector[ScalarRes]], sc)

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

  private def simReadBuffer(buf: ReadBuffer[?], sc: SimContext): (Result, SimContext) = buf match
    case ReadBuffer(buffer, index) =>
      val (res, sc1) = sim(index.tree, sc)
      val i = res.asInstanceOf[Int]
      val newSc = sc1.addRead(buffer, i)
      (newSc.lookupRead(buffer, i), newSc)

  private def simReadUniform(uni: ReadUniform[?], sc: SimContext): (Result, SimContext) = uni match
    case ReadUniform(uniform) => ???

  private def simGArrayElem(gElem: GArrayElem[?], sc: SimContext): (Result, SimContext) = gElem match
    case GArrayElem(index, i) => ???

  private def simFoldSeq(seq: FoldSeq[?, ?], sc: SimContext): (Result, SimContext) = seq match
    case FoldSeq(zero, fn, seq) => ???

  private def simComposeStruct(cs: ComposeStruct[?], sc: SimContext): (Result, SimContext) = cs match
    case ComposeStruct(fields, resultSchema) => ???

  private def simGetField(gf: GetField[?, ?], sc: SimContext): (Result, SimContext) = gf match
    case GetField(struct, fieldIndex) => ???
