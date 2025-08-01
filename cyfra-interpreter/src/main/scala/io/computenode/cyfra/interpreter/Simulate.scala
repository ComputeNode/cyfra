package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, macros.FnCall.FnIdentifier, control.Scope
import collections.*, GArray.GArrayElem, GSeq.{CurrentElem, AggregateElem, FoldSeq}
import struct.*, GStruct.{ComposeStruct, GetField}
import io.computenode.cyfra.spirv.BlockBuilder.buildBlock

object Simulate:
  import Result.*

  def sim(v: Value, records: Records = Map())(using sc: SimContext = SimContext()): SimRes = sim(v.tree, records)
  def sim(e: Expression[?], records: Records)(using SimContext): SimRes = simIterate(buildBlock(e), records)

  @annotation.tailrec
  def simIterate(blocks: List[Expression[?]], records: Records, results: Results = Map())(using sc: SimContext): SimRes = blocks match
    case head :: next => // reads have to be treated specially, since they will update the records
      val (newResults, records1) = head match
        case e: ReadBuffer[?]  => simReadBuffer(e, records) // records updated with reads
        case e: ReadUniform[?] => simReadUniform(e, records) // records updated with reads
        case e: WhenExpr[?]    => simWhen(e, records)
        case _                 => (simOne(head)(using records), records) // no reads, records not updated
      val newRecords = records1.updateResults(head.treeid, newResults) // update caches with new results
      simIterate(next, newRecords, newResults)
    case Nil => SimRes(results, records, sc)

  // in these cases, the records don't change since there are no reads.
  def simOne(e: Expression[?])(using records: Records, sc: SimContext): Results = e match
    case e: PhantomExpression[?]      => simPhantom(e)
    case Negate(a)                    => simValue(a).view.mapValues(_.negate).toMap
    case e: BinaryOpExpression[?]     => simBinOp(e)
    case ScalarProd(a, b)             => simVector(a).join(simScalar(b))(_.scale(_))
    case DotProd(a, b)                => simVector(a).join(simVector(b))(_.dot(_))
    case e: BitwiseOpExpression[?]    => simBitwiseOp(e)
    case e: ComparisonOpExpression[?] => simCompareOp(e)
    case And(a, b)                    => simScalar(a).join(simScalar(b))(_ && _)
    case Or(a, b)                     => simScalar(a).join(simScalar(b))(_ || _)
    case Not(a)                       => simScalar(a).view.mapValues(_.negate).toMap
    case ExtractScalar(a, i)          =>
      val (aRes, iRes) = (simVector(a), simValue(i))
      aRes.map((invocId, vector) => invocId -> vector.apply(iRes(invocId).asInstanceOf[Int]))
    case e: ConvertExpression[?, ?] => simConvert(e)
    case e: Const[?]                => simConst(e)
    case ComposeVec2(a, b)          =>
      val (aRes, bRes) = (simScalar(a), simScalar(b))
      aRes.map((invocId, ar) => invocId -> Vector(ar, bRes(invocId)))
    case ComposeVec3(a, b, c) =>
      val (aRes, bRes, cRes) = (simScalar(a), simScalar(b), simScalar(c))
      records.keys
        .map: invocId =>
          invocId -> Vector(aRes(invocId), bRes(invocId), cRes(invocId))
        .toMap
    case ComposeVec4(a, b, c, d) =>
      val (aRes, bRes, cRes, dRes) = (simScalar(a), simScalar(b), simScalar(c), simScalar(d))
      records.keys
        .map: invocId =>
          invocId -> Vector(aRes(invocId), bRes(invocId), cRes(invocId), dRes(invocId))
        .toMap
    case ExtFunctionCall(fn, args)    => ??? // simExtFunc(fn, args.map(simValue)
    case FunctionCall(fn, body, args) => ??? // simFunc(fn, simScope(body), args.map(simValue)
    case InvocationId                 => simInvocId(records)
    case Pass(value)                  => ???
    case Dynamic(source)              => ???
    case e: GArrayElem[?]             => simGArrayElem(e)
    case e: FoldSeq[?, ?]             => simFoldSeq(e)
    case e: ComposeStruct[?]          => simComposeStruct(e)
    case e: GetField[?, ?]            => simGetField(e)
    case _                            => throw IllegalArgumentException("sim: wrong argument")

  private def simPhantom(e: PhantomExpression[?])(using Records): Results = e match
    case CurrentElem(tid: Int)   => ???
    case AggregateElem(tid: Int) => ???

  private def simBinOp(e: BinaryOpExpression[?])(using Records): Results = e match
    case Sum(a, b)  => simValue(a).join(simValue(b))(_.add(_)) // scalar or vector
    case Diff(a, b) => simValue(a).join(simValue(b))(_.sub(_)) // scalar or vector
    case Mul(a, b)  => simScalar(a).join(simScalar(b))(_.mul(_))
    case Div(a, b)  => simScalar(a).join(simScalar(b))(_.div(_))
    case Mod(a, b)  => simScalar(a).join(simScalar(b))(_.mod(_))

  private def simBitwiseOp(e: BitwiseOpExpression[?])(using Records): Results = e match
    case e: BitwiseBinaryOpExpression[?] => simBitwiseBinOp(e)
    case BitwiseNot(a)                   => simScalar(a).view.mapValues(_.bitNeg).toMap
    case ShiftLeft(a, by)                => simScalar(a).join(simScalar(by))(_.shiftLeft(_))
    case ShiftRight(a, by)               => simScalar(a).join(simScalar(by))(_.shiftRight(_))

  private def simBitwiseBinOp(e: BitwiseBinaryOpExpression[?])(using Records): Results = e match
    case BitwiseAnd(a, b) => simScalar(a).join(simScalar(b))(_.bitAnd(_))
    case BitwiseOr(a, b)  => simScalar(a).join(simScalar(b))(_.bitOr(_))
    case BitwiseXor(a, b) => simScalar(a).join(simScalar(b))(_.bitXor(_))

  private def simCompareOp(e: ComparisonOpExpression[?])(using Records): Results = e match
    case GreaterThan(a, b)      => simScalar(a).join(simScalar(b))(_.gt(_))
    case LessThan(a, b)         => simScalar(a).join(simScalar(b))(_.lt(_))
    case GreaterThanEqual(a, b) => simScalar(a).join(simScalar(b))(_.gteq(_))
    case LessThanEqual(a, b)    => simScalar(a).join(simScalar(b))(_.lteq(_))
    case Equal(a, b)            => simScalar(a).join(simScalar(b))(_.eql(_))

  private def simConvert(e: ConvertExpression[?, ?])(using records: Records): Results = e match
    case ToFloat32(a) => records.view.mapValues(_.cache(a.treeid).asInstanceOf[Float]).toMap
    case ToInt32(a)   => records.view.mapValues(_.cache(a.treeid).asInstanceOf[Int]).toMap
    case ToUInt32(a)  => records.view.mapValues(_.cache(a.treeid).asInstanceOf[Int]).toMap

  private def simConst(e: Const[?])(using records: Records): Results = e match
    case ConstFloat32(value) => records.view.mapValues(_ => value).toMap
    case ConstInt32(value)   => records.view.mapValues(_ => value).toMap
    case ConstUInt32(value)  => records.view.mapValues(_ => value).toMap
    case ConstGB(value)      => records.view.mapValues(_ => value).toMap

  private def simValue(v: Value)(using Records): Results = v match
    case v: Scalar => simScalar(v)
    case v: Vec[?] => simVector(v)

  private def simScalar(v: Scalar)(using records: Records): Map[InvocId, ScalarRes] = v match
    case v: FloatType     => records.view.mapValues(_.cache(v.tree.treeid).asInstanceOf[Float]).toMap
    case v: IntType       => records.view.mapValues(_.cache(v.tree.treeid).asInstanceOf[Int]).toMap
    case v: UIntType      => records.view.mapValues(_.cache(v.tree.treeid).asInstanceOf[Int]).toMap
    case GBoolean(source) => records.view.mapValues(_.cache(source.treeid).asInstanceOf[Boolean]).toMap

  private def simVector(v: Vec[?])(using records: Records): Map[InvocId, Vector[ScalarRes]] = v match
    case Vec2(tree) => records.view.mapValues(_.cache(tree.treeid).asInstanceOf[Vector[ScalarRes]]).toMap
    case Vec3(tree) => records.view.mapValues(_.cache(tree.treeid).asInstanceOf[Vector[ScalarRes]]).toMap
    case Vec4(tree) => records.view.mapValues(_.cache(tree.treeid).asInstanceOf[Vector[ScalarRes]]).toMap

  private def simExtFunc(fn: FunctionName, args: List[Result], records: Records): (Result, Records) = ???
  private def simFunc(fn: FnIdentifier, body: Result, args: List[Result], records: Records): (Result, Records) = ???
  private def simInvocId(records: Records): Map[InvocId, InvocId] = records.map((invocId, _) => invocId -> invocId)

  // @annotation.tailrec
  private def whenHelper(
    when: Expression[GBoolean],
    thenCode: Scope[?],
    otherConds: List[Scope[GBoolean]],
    otherCaseCodes: List[Scope[?]],
    otherwise: Scope[?],
    resultsSoFar: Results,
    finishedRecords: Records,
    pendingRecords: Records,
  )(using SimContext): (Results, Records) =
    if pendingRecords.isEmpty then (resultsSoFar, finishedRecords)
    else
      // scopes are not included in caches, they have to be simulated from scratch.
      // there could be reads happening in scopes, records have to be updated.
      // scopes can still read from the outer SimContext.
      val SimRes(boolResults, records1, _) = sim(when, pendingRecords) // SimContext does not change.

      // Split invocations that enter this branch.
      val (enterRecords, newPendingRecords) = records1.partition((invocId, _) => boolResults(invocId).asInstanceOf[Boolean])

      // Only those invocs that enter the branch will have their records updated with thenCode result.
      val SimRes(thenResults, thenRecords, _) = sim(thenCode.expr, enterRecords)

      otherConds.headOption match
        case None => // run pending invocs on otherwise, collect all results and records, done
          val SimRes(owResults, owRecords, _) = sim(otherwise.expr, newPendingRecords)
          (resultsSoFar ++ thenResults ++ owResults, finishedRecords ++ thenRecords ++ owRecords)
        case Some(cond) =>
          whenHelper(
            when = cond.expr,
            thenCode = otherCaseCodes.head,
            otherConds = otherConds.tail,
            otherCaseCodes = otherCaseCodes.tail,
            otherwise = otherwise,
            resultsSoFar = resultsSoFar ++ thenResults,
            finishedRecords = finishedRecords ++ thenRecords,
            pendingRecords = newPendingRecords,
          )

  private def simWhen(e: WhenExpr[?], records: Records)(using SimContext): (Results, Records) = e match
    case WhenExpr(when, thenCode, otherConds, otherCaseCodes, otherwise) =>
      whenHelper(when.tree, thenCode, otherConds, otherCaseCodes, otherwise, Map(), Map(), records)

  private def simReadBuffer(e: ReadBuffer[?], records: Records)(using sc: SimContext): (Results, Records) = e match
    case ReadBuffer(buffer, index) =>
      val indices = records.view.mapValues(_.cache(index.tree.treeid).asInstanceOf[Int]).toMap
      val readValues = indices.view.mapValues(i => sc.lookup(buffer, i)).toMap
      val newRecords = records.map: (invocId, record) =>
        invocId -> record.addRead(ReadBuf(e.treeid, buffer, indices(invocId), readValues(invocId)))
      (readValues, newRecords)

  private def simReadUniform(e: ReadUniform[?], records: Records)(using sc: SimContext): (Results, Records) = e match
    case ReadUniform(uniform) =>
      val readValue = sc.lookupUni(uniform) // same for all invocs
      val newResults = records.map((invocId, _) => invocId -> readValue)
      val newRecords = records.map: (invocId, record) =>
        invocId -> record.addRead(ReadUni(e.treeid, uniform, readValue))
      (newResults, newRecords)

  private def simGArrayElem(gElem: GArrayElem[?]): Results = gElem match
    case GArrayElem(index, i) => ???

  private def simFoldSeq(seq: FoldSeq[?, ?]): Results = seq match
    case FoldSeq(zero, fn, seq) => ???

  private def simComposeStruct(cs: ComposeStruct[?]): Results = cs match
    case ComposeStruct(fields, resultSchema) => ???

  private def simGetField(gf: GetField[?, ?]): Results = gf match
    case GetField(struct, fieldIndex) => ???
