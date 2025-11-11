package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.*, macros.FnCall.FnIdentifier, control.Scope
import collections.*, GSeq.{CurrentElem, AggregateElem, FoldSeq}
import struct.*, GStruct.{ComposeStruct, GetField}
import io.computenode.cyfra.spirv.BlockBuilder.buildBlock

object Simulate:
  import Result.*

  // Helpful overload to simulate values instead of expressions
  def sim(v: Value, sc: SimContext): SimContext = sim(v.tree, sc)

  // for evaluating expressions that don't cause any writes (therefore don't change data)
  def sim(e: Expression[?], sc: SimContext): SimContext = simIterate(buildBlock(e), sc)

  @annotation.tailrec
  def simIterate(blocks: List[Expression[?]], sc: SimContext): SimContext =
    val SimContext(results, records, data, profs) = sc
    blocks match
      case head :: next =>
        val SimContext(newResults, records1, _, newProfs) = head match
          case e: ReadBuffer[?]  => simReadBuffer(e, sc)
          case e: ReadUniform[?] =>
            val (res, rec) = simReadUniform(e, records)(using data)
            SimContext(res, rec, data, profs)
          case e: WhenExpr[?] => simWhen(e, sc)
          case _              => SimContext(simOne(head)(using records, data), records, data, profs)
        val newRecords = records1.updateResults(head.treeid, newResults) // update caches with new results
        simIterate(next, SimContext(newResults, newRecords, data, newProfs))
      case Nil => sc

  // in these cases, the records don't change since there are no reads.
  def simOne(e: Expression[?])(using records: Records, data: SimData): Results = e match
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
    case ExtFunctionCall(fn, args)    => ??? // simExtFunc(fn, args.map(simValue))
    case FunctionCall(fn, body, args) => ??? // simFunc(fn, simScope(body), args.map(simValue))
    case InvocationId                 => simInvocId(records)
    case Pass(value)                  => ???
    // case Dynamic(source)              => ???
    // case e: GArrayElem[?]    => simGArrayElem(e)
    case e: FoldSeq[?, ?]    => simFoldSeq(e)
    case e: ComposeStruct[?] => simComposeStruct(e)
    case e: GetField[?, ?]   => simGetField(e)
    case _                   => throw IllegalArgumentException("sim: wrong argument")

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

  private def simExtFunc(fn: FunctionName, args: List[Result], records: Records): Results = ???
  private def simFunc(fn: FnIdentifier, body: Result, args: List[Result], records: Records): Results = ???
  private def simInvocId(records: Records): Map[InvocId, InvocId] = records.map((invocId, _) => invocId -> invocId)

  @annotation.tailrec
  private def whenHelper(
    when: Expression[GBoolean],
    thenCode: Scope[?],
    otherConds: List[Scope[GBoolean]],
    otherCaseCodes: List[Scope[?]],
    otherwise: Scope[?],
    resultsSoFar: Results,
    finishedRecords: Records,
    pendingRecords: Records,
    sc: SimContext,
  )(using rootTreeId: TreeId): SimContext =
    if pendingRecords.isEmpty then sc
    else
      // scopes are not included in caches, they have to be simulated from scratch.
      // there could be reads happening in scopes, records have to be updated.
      // scopes can still read from the outer SimData.
      val pendingSc = SimContext(Map(), pendingRecords, sc.data, sc.profs)
      val SimContext(boolResults, boolRecords, boolData, boolProfs) = sim(when, pendingSc)

      // Split invocations that enter this branch.
      val (enterRecords, pendingRecords1) = boolRecords.partition((invocId, _) => boolResults(invocId).asInstanceOf[Boolean])

      // Finished records and still pending records will idle.
      val newFinishedRecords = finishedRecords.updateIdles(rootTreeId)
      val newPendingRecords = pendingRecords1.updateIdles(rootTreeId)

      // Only those invocs that enter the branch will have their records updated with thenCode result.
      val enterSc = SimContext(Map(), enterRecords, boolData, boolProfs)
      val thenSc = sim(thenCode.expr, enterSc)
      val SimContext(thenResults, thenRecords, thenData, thenProfs) = thenSc

      otherConds.headOption match
        case None => // run pending invocs on otherwise, collect all results and records, done
          val newPendingSc = SimContext(Map(), newPendingRecords, thenData, thenProfs)
          val SimContext(owResults, owRecords, owData, owProfs) = sim(otherwise.expr, newPendingSc)
          SimContext(resultsSoFar ++ thenResults ++ owResults, finishedRecords ++ thenRecords ++ owRecords, owData, owProfs)
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
            sc = thenSc,
          )

  private def simWhen(e: WhenExpr[?], sc: SimContext): SimContext = e match
    case WhenExpr(when, thenCode, otherConds, otherCaseCodes, otherwise) =>
      whenHelper(when.tree, thenCode, otherConds, otherCaseCodes, otherwise, Map(), Map(), sc.records, sc)(using e.treeid)

  private def simReadBuffer(e: ReadBuffer[?], sc: SimContext): SimContext =
    val SimContext(_, records, data, profs) = sc
    e match
      case ReadBuffer(buffer, index) =>
        val indices = records.view.mapValues(_.cache(index.tree.treeid).asInstanceOf[Int]).toMap
        // println(s"$e: $indices")
        val readValues = indices.view.mapValues(i => data.lookup(buffer, i)).toMap
        val newRecords = records.map: (invocId, record) =>
          invocId -> record.addRead(ReadBuf(e.treeid, buffer, indices(invocId), readValues(invocId)))

        // check if the read addresses coalesced or not
        val addresses = indices.values.toSeq
        val profile = ReadProfile(e.treeid, addresses)
        val coalesceProfile = CoalesceProfile(addresses, profile)

        SimContext(readValues, newRecords, data, coalesceProfile :: profs)

  private def simReadUniform(e: ReadUniform[?], records: Records)(using data: SimData): (Results, Records) = e match
    case ReadUniform(uniform) =>
      val readValue = data.lookupUni(uniform) // same for all invocs
      val newResults = records.map((invocId, _) => invocId -> readValue)
      val newRecords = records.map: (invocId, record) =>
        invocId -> record.addRead(ReadUni(e.treeid, uniform, readValue))
      (newResults, newRecords)

  // private def simGArrayElem(gElem: GArrayElem[?]): Results = gElem match
  //   case GArrayElem(index, i) => ???

  private def simFoldSeq(seq: FoldSeq[?, ?]): Results = seq match
    case FoldSeq(zero, fn, seq) => ???

  private def simComposeStruct(cs: ComposeStruct[?]): Results = cs match
    case ComposeStruct(fields, resultSchema) => ???

  private def simGetField(gf: GetField[?, ?]): Results = gf match
    case GetField(struct, fieldIndex) => ???
