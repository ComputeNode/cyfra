package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.binding.*
import io.computenode.cyfra.dsl.collections.GSeq
import io.computenode.cyfra.dsl.macros.Source
import io.computenode.cyfra.dsl.struct.GStruct.{ComposeStruct, GetField}
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.spirv.SpirvTypes.*
import io.computenode.cyfra.spirv.compilers.ExtFunctionCompiler.compileExtFunctionCall
import io.computenode.cyfra.spirv.compilers.FunctionCompiler.compileFunctionCall
import io.computenode.cyfra.spirv.compilers.WhenCompiler.compileWhen
import io.computenode.cyfra.spirv.{BlockBuilder, Context}
import izumi.reflect.Tag

import scala.annotation.tailrec

private[cyfra] object ExpressionCompiler:

  val WorkerIndexTag = "worker_index"

  private def binaryOpOpcode(expr: BinaryOpExpression[?]) = expr match
    case _: Sum[?]  => (Op.OpIAdd, Op.OpFAdd)
    case _: Diff[?] => (Op.OpISub, Op.OpFSub)
    case _: Mul[?]  => (Op.OpIMul, Op.OpFMul)
    case _: Div[?]  => (Op.OpSDiv, Op.OpFDiv)
    case _: Mod[?]  => (Op.OpSMod, Op.OpFMod)

  private def compileBinaryOpExpression(bexpr: BinaryOpExpression[?], ctx: Context): (List[Instruction], Context) =
    val tpe = bexpr.tag
    val typeRef = ctx.valueTypeMap(tpe.tag)
    val subOpcode = tpe match
      case i
          if i.tag <:< summon[Tag[IntType]].tag || i.tag <:< summon[Tag[UIntType]].tag ||
            (i.tag <:< summon[Tag[Vec[?]]].tag && i.tag.typeArgs.head <:< summon[Tag[IntType]].tag) =>
        binaryOpOpcode(bexpr)._1
      case f if f.tag <:< summon[Tag[FloatType]].tag || (f.tag <:< summon[Tag[Vec[?]]].tag && f.tag.typeArgs.head <:< summon[Tag[FloatType]].tag) =>
        binaryOpOpcode(bexpr)._2
    val instructions = List(
      Instruction(
        subOpcode,
        List(ResultRef(typeRef), ResultRef(ctx.nextResultId), ResultRef(ctx.exprRefs(bexpr.a.treeid)), ResultRef(ctx.exprRefs(bexpr.b.treeid))),
      ),
    )
    val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (bexpr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
    (instructions, updatedContext)

  private def compileConvertExpression(cexpr: ConvertExpression[?, ?], ctx: Context): (List[Instruction], Context) =
    val tpe = cexpr.tag
    val typeRef = ctx.valueTypeMap(tpe.tag)
    val tfOpcode = (cexpr.fromTag, cexpr) match
      case (from, _: ToFloat32[?]) if from.tag =:= Int32Tag.tag  => Op.OpConvertSToF
      case (from, _: ToFloat32[?]) if from.tag =:= UInt32Tag.tag => Op.OpConvertUToF
      case (from, _: ToInt32[?]) if from.tag =:= Float32Tag.tag  => Op.OpConvertFToS
      case (from, _: ToUInt32[?]) if from.tag =:= Float32Tag.tag => Op.OpConvertFToU
      case (from, _: ToInt32[?]) if from.tag =:= UInt32Tag.tag   => Op.OpBitcast
      case (from, _: ToUInt32[?]) if from.tag =:= Int32Tag.tag   => Op.OpBitcast
    val instructions = List(Instruction(tfOpcode, List(ResultRef(typeRef), ResultRef(ctx.nextResultId), ResultRef(ctx.exprRefs(cexpr.a.treeid)))))
    val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (cexpr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
    (instructions, updatedContext)

  def comparisonOp(comparisonOpExpression: ComparisonOpExpression[?]) =
    comparisonOpExpression match
      case _: GreaterThan[?]      => (Op.OpSGreaterThan, Op.OpFOrdGreaterThan)
      case _: LessThan[?]         => (Op.OpSLessThan, Op.OpFOrdLessThan)
      case _: GreaterThanEqual[?] => (Op.OpSGreaterThanEqual, Op.OpFOrdGreaterThanEqual)
      case _: LessThanEqual[?]    => (Op.OpSLessThanEqual, Op.OpFOrdLessThanEqual)
      case _: Equal[?]            => (Op.OpIEqual, Op.OpFOrdEqual)

  private def compileBitwiseExpression(bexpr: BitwiseOpExpression[?], ctx: Context): (List[Instruction], Context) =
    val tpe = bexpr.tag
    val typeRef = ctx.valueTypeMap(tpe.tag)
    val subOpcode = bexpr match
      case _: BitwiseAnd[?] => Op.OpBitwiseAnd
      case _: BitwiseOr[?]  => Op.OpBitwiseOr
      case _: BitwiseXor[?] => Op.OpBitwiseXor
      case _: BitwiseNot[?] => Op.OpNot
      case _: ShiftLeft[?]  => Op.OpShiftLeftLogical
      case _: ShiftRight[?] => Op.OpShiftRightLogical
    val instructions = List(
      Instruction(subOpcode, List(ResultRef(typeRef), ResultRef(ctx.nextResultId)) ::: bexpr.exprDependencies.map(d => ResultRef(ctx.exprRefs(d.treeid)))),
    )
    val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (bexpr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
    (instructions, updatedContext)

  def compileBlock(tree: E[?], ctx: Context): (List[Words], Context) =

    @tailrec
    def compileExpressions(exprs: List[E[?]], ctx: Context, acc: List[Words]): (List[Words], Context) =
      if exprs.isEmpty then (acc, ctx)
      else
        val expr = exprs.head
        if ctx.exprRefs.contains(expr.treeid) then compileExpressions(exprs.tail, ctx, acc)
        else

          val name: Option[String] = expr.of match
            case Some(v) => Some(v.source.name)
            case _       => None

          val (instructions, updatedCtx) = expr match
            case c @ Const(x) =>
              val constRef = ctx.constRefs((c.tag, x))
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (c.treeid -> constRef))
              (List(), updatedContext)

            case w @ WorkerIndex =>
              (Nil, ctx.copy(exprRefs = ctx.exprRefs + (w.treeid -> ctx.workerIndexRef)))

            case d @ Binding(id) =>
              (Nil, ctx.copy(exprRefs = ctx.exprRefs + (d.treeid -> ctx.uniformVarRefs(id))))

            case c: ConvertExpression[?, ?] =>
              compileConvertExpression(c, ctx)

            case b: BinaryOpExpression[?] =>
              compileBinaryOpExpression(b, ctx)

            case negate: Negate[?] =>
              val op =
                if negate.tag.tag <:< summon[Tag[FloatType]].tag ||
                  (negate.tag.tag <:< summon[Tag[Vec[?]]].tag && negate.tag.tag.typeArgs.head <:< summon[Tag[FloatType]].tag) then Op.OpFNegate
                else Op.OpSNegate
              val instructions = List(
                Instruction(op, List(ResultRef(ctx.valueTypeMap(negate.tag.tag)), ResultRef(ctx.nextResultId), ResultRef(ctx.exprRefs(negate.a.treeid)))),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (negate.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case bo: BitwiseOpExpression[?] =>
              compileBitwiseExpression(bo, ctx)

            case and: And =>
              val instructions = List(
                Instruction(
                  Op.OpLogicalAnd,
                  List(
                    ResultRef(ctx.valueTypeMap(GBooleanTag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(and.a.treeid)),
                    ResultRef(ctx.exprRefs(and.b.treeid)),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (and.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case or: Or =>
              val instructions = List(
                Instruction(
                  Op.OpLogicalOr,
                  List(
                    ResultRef(ctx.valueTypeMap(GBooleanTag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(or.a.treeid)),
                    ResultRef(ctx.exprRefs(or.b.treeid)),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (or.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case not: Not =>
              val instructions = List(
                Instruction(
                  Op.OpLogicalNot,
                  List(ResultRef(ctx.valueTypeMap(GBooleanTag.tag)), ResultRef(ctx.nextResultId), ResultRef(ctx.exprRefs(not.a.treeid))),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case sp: ScalarProd[?, ?] =>
              val instructions = List(
                Instruction(
                  Op.OpVectorTimesScalar,
                  List(
                    ResultRef(ctx.valueTypeMap(sp.tag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(sp.a.treeid)),
                    ResultRef(ctx.exprRefs(sp.b.treeid)),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case dp: DotProd[?, ?] =>
              val instructions = List(
                Instruction(
                  Op.OpDot,
                  List(
                    ResultRef(ctx.valueTypeMap(dp.tag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(dp.a.treeid)),
                    ResultRef(ctx.exprRefs(dp.b.treeid)),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (dp.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case co: ComparisonOpExpression[?] =>
              val (intOp, floatOp) = comparisonOp(co)
              val op = if co.operandTag.tag <:< summon[Tag[FloatType]].tag then floatOp else intOp
              val instructions = List(
                Instruction(
                  op,
                  List(
                    ResultRef(ctx.valueTypeMap(GBooleanTag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(co.a.treeid)),
                    ResultRef(ctx.exprRefs(co.b.treeid)),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case e: ExtractScalar[?, ?] =>
              val instructions = List(
                Instruction(
                  Op.OpVectorExtractDynamic,
                  List(
                    ResultRef(ctx.valueTypeMap(e.tag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(e.a.treeid)),
                    ResultRef(ctx.exprRefs(e.i.treeid)),
                  ),
                ),
              )

              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case composeVec2: ComposeVec2[?] =>
              val instructions = List(
                Instruction(
                  Op.OpCompositeConstruct,
                  List(
                    ResultRef(ctx.valueTypeMap(composeVec2.tag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(composeVec2.a.treeid)),
                    ResultRef(ctx.exprRefs(composeVec2.b.treeid)),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case composeVec3: ComposeVec3[?] =>
              val instructions = List(
                Instruction(
                  Op.OpCompositeConstruct,
                  List(
                    ResultRef(ctx.valueTypeMap(composeVec3.tag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(composeVec3.a.treeid)),
                    ResultRef(ctx.exprRefs(composeVec3.b.treeid)),
                    ResultRef(ctx.exprRefs(composeVec3.c.treeid)),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case composeVec4: ComposeVec4[?] =>
              val instructions = List(
                Instruction(
                  Op.OpCompositeConstruct,
                  List(
                    ResultRef(ctx.valueTypeMap(composeVec4.tag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(composeVec4.a.treeid)),
                    ResultRef(ctx.exprRefs(composeVec4.b.treeid)),
                    ResultRef(ctx.exprRefs(composeVec4.c.treeid)),
                    ResultRef(ctx.exprRefs(composeVec4.d.treeid)),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (instructions, updatedContext)

            case fc: ExtFunctionCall[?] =>
              compileExtFunctionCall(fc, ctx)

            case fc: FunctionCall[?] =>
              compileFunctionCall(fc, ctx)

            case ReadBuffer(buffer, i) =>
              val instructions = List(
                Instruction(
                  Op.OpAccessChain,
                  List(
                    ResultRef(ctx.uniformPointerMap(ctx.valueTypeMap(buffer.tag.tag))),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.bufferBlocks(buffer).blockVarRef),
                    ResultRef(ctx.constRefs((Int32Tag, 0))),
                    ResultRef(ctx.exprRefs(i.treeid)),
                  ),
                ),
                Instruction(Op.OpLoad, List(IntWord(ctx.valueTypeMap(buffer.tag.tag)), ResultRef(ctx.nextResultId + 1), ResultRef(ctx.nextResultId))),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> (ctx.nextResultId + 1)), nextResultId = ctx.nextResultId + 2)
              (instructions, updatedContext)

            case when: WhenExpr[?] =>
              compileWhen(when, ctx)

            case fd: GSeq.FoldSeq[?, ?] =>
              GSeqCompiler.compileFold(fd, ctx)

            case cs: ComposeStruct[?] =>
              // noinspection ScalaRedundantCast
              val schema = cs.resultSchema.asInstanceOf[GStructSchema[?]]
              val fields = cs.fields
              val insns: List[Instruction] = List(
                Instruction(
                  Op.OpCompositeConstruct,
                  List(ResultRef(ctx.valueTypeMap(cs.tag.tag)), ResultRef(ctx.nextResultId)) ::: fields.zipWithIndex.map { case (f, i) =>
                    ResultRef(ctx.exprRefs(cs.exprDependencies(i).treeid))
                  },
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (cs.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (insns, updatedContext)
              
            case gf @ GetField(binding @ Binding(bindingId), fieldIndex) =>
              val insns: List[Instruction] = List(
                Instruction(
                  Op.OpAccessChain,
                  List(
                    ResultRef(ctx.uniformPointerMap(ctx.valueTypeMap(gf.tag.tag))),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.uniformVarRefs(bindingId)),
                    ResultRef(ctx.constRefs((Int32Tag, gf.fieldIndex))),
                  ),
                ),
                Instruction(Op.OpLoad, List(IntWord(ctx.valueTypeMap(gf.tag.tag)), ResultRef(ctx.nextResultId + 1), ResultRef(ctx.nextResultId))),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> (ctx.nextResultId + 1)), nextResultId = ctx.nextResultId + 2)
              (insns, updatedContext)
            case gf: GetField[?, ?] =>
              val insns: List[Instruction] = List(
                Instruction(
                  Op.OpCompositeExtract,
                  List(
                    ResultRef(ctx.valueTypeMap(gf.tag.tag)),
                    ResultRef(ctx.nextResultId),
                    ResultRef(ctx.exprRefs(gf.struct.treeid)),
                    IntWord(gf.fieldIndex),
                  ),
                ),
              )
              val updatedContext = ctx.copy(exprRefs = ctx.exprRefs + (expr.treeid -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
              (insns, updatedContext)

            case ph: PhantomExpression[?] => (List(), ctx)
          val ctxWithName = updatedCtx.copy(exprNames = updatedCtx.exprNames ++ name.map(n => (updatedCtx.nextResultId - 1, n)).toMap)
          compileExpressions(exprs.tail, ctxWithName, acc ::: instructions)
    val sortedTree = BlockBuilder.buildBlock(tree, providedExprIds = ctx.exprRefs.keySet)
    compileExpressions(sortedTree, ctx, Nil)
