package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.gio.GIO.{ConditionalWhen, CurrentFoldRepeatAcc, CurrentRepeatIndex, FlatMap, FoldRepeat, Printf, Pure, Repeat, WorkgroupBarrier}
import io.computenode.cyfra.dsl.binding.{WriteBuffer, WriteShared}
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.spirv.SpirvConstants.{DEBUG_PRINTF_REF, TYPE_VOID_REF}
import io.computenode.cyfra.spirv.SpirvTypes.{GBooleanTag, Int32Tag}

import scala.collection.mutable

object GIOCompiler:

  def compileGio(gio: GIO[?], ctx: Context, acc: List[Words] = Nil): (List[Words], Context) =
    gio match

      case Pure(v) =>
        val (insts, updatedCtx) = ExpressionCompiler.compileBlock(v.tree, ctx)
        (acc ::: insts, updatedCtx)

      case wb @ WriteBuffer(buffer, index, value) =>
        val (valueInsts, ctxWithValue) = ExpressionCompiler.compileBlock(value.tree, ctx)
        val (indexInsts, ctxWithIndex) = ExpressionCompiler.compileBlock(index.tree, ctxWithValue)
        // Compile the underlying Empty to register it for FoldRepeat body lookup
        val (underlyingInsts, ctxWithUnderlying) = ExpressionCompiler.compileBlock(wb.underlying.tree, ctxWithIndex)
        val insns = List(
          Instruction(
            Op.OpAccessChain,
            List(
              ResultRef(ctxWithUnderlying.uniformPointerMap(ctxWithUnderlying.valueTypeMap(buffer.tag.tag))),
              ResultRef(ctxWithUnderlying.nextResultId),
              ResultRef(ctxWithUnderlying.bufferBlocks(buffer).blockVarRef),
              ResultRef(ctxWithUnderlying.constRefs((Int32Tag, 0))),
              ResultRef(ctxWithUnderlying.exprRefs(index.tree.treeid)),
            ),
          ),
          Instruction(Op.OpStore, List(ResultRef(ctxWithUnderlying.nextResultId), ResultRef(ctxWithUnderlying.exprRefs(value.tree.treeid)))),
        )
        val updatedCtx = ctxWithUnderlying.copy(nextResultId = ctxWithUnderlying.nextResultId + 1)
        // valueInsts before indexInsts: value compiled first, may define exprs index uses
        (acc ::: valueInsts ::: indexInsts ::: underlyingInsts ::: insns, updatedCtx)

      case FlatMap(v, n) =>
        val (vInsts, ctxAfterV) = compileGio(v, ctx, acc)
        compileGio(n, ctxAfterV, vInsts)

      case r @ Repeat(n, f, unroll) =>
        compileRepeat(n, f, unroll, ctx, acc)

      case fr: FoldRepeat[?] =>
        compileFoldRepeat(fr, ctx, acc)

      case WorkgroupBarrier =>
        val scopeId = ctx.constRefs((Int32Tag, Scope.Workgroup.opcode))
        val semanticsId = ctx.constRefs((Int32Tag, MemorySemantics.WorkgroupMemory.opcode | MemorySemantics.AcquireRelease.opcode))
        val barrierInsn = Instruction(
          Op.OpControlBarrier,
          List(ResultRef(scopeId), ResultRef(scopeId), ResultRef(semanticsId)),
        )
        (acc ::: List(barrierInsn), ctx)

      case ConditionalWhen(cond, body) =>
        compileConditionalWhen(cond, body, ctx, acc)

      case WriteShared(buffer, index, value) =>
        val sharedId = buffer.asInstanceOf[io.computenode.cyfra.dsl.binding.GShared.GSharedImpl[?]].sharedId
        val (valueInsts, ctxWithValue) = ExpressionCompiler.compileBlock(value.tree, ctx)
        val (indexInsts, ctxWithIndex) = ExpressionCompiler.compileBlock(index.tree, ctxWithValue)
        val sharedBlock = ctxWithIndex.sharedVarRefs(sharedId)
        val insns = List(
          Instruction(
            Op.OpAccessChain,
            List(
              ResultRef(sharedBlock.pointerTypeRef),
              ResultRef(ctxWithIndex.nextResultId),
              ResultRef(sharedBlock.varRef),
              ResultRef(ctxWithIndex.exprRefs(index.tree.treeid)),
            ),
          ),
          Instruction(Op.OpStore, List(ResultRef(ctxWithIndex.nextResultId), ResultRef(ctxWithIndex.exprRefs(value.tree.treeid)))),
        )
        val updatedCtx = ctxWithIndex.copy(nextResultId = ctxWithIndex.nextResultId + 1)
        // valueInsts before indexInsts: value compiled first, may define exprs index uses
        (acc ::: valueInsts ::: indexInsts ::: insns, updatedCtx)

      case Printf(format, args*) =>
        val (argsInsts, ctxAfterArgs) = args.foldLeft((List.empty[Words], ctx)) { case ((instsAcc, cAcc), arg) =>
          val (argInsts, cAfterArg) = ExpressionCompiler.compileBlock(arg.tree, cAcc)
          (instsAcc ::: argInsts, cAfterArg)
        }
        val argResults = args.map(a => ResultRef(ctxAfterArgs.exprRefs(a.tree.treeid))).toList
        val printf = Instruction(
          Op.OpExtInst,
          List(
            ResultRef(TYPE_VOID_REF),
            ResultRef(ctxAfterArgs.nextResultId),
            ResultRef(DEBUG_PRINTF_REF),
            IntWord(1),
            ResultRef(ctx.stringLiterals(format)),
          ) ::: argResults,
        )
        (acc ::: argsInsts ::: List(printf), ctxAfterArgs.copy(nextResultId = ctxAfterArgs.nextResultId + 1))

  private def compileRepeat(
    n: io.computenode.cyfra.dsl.Value.Int32,
    f: GIO[?],
    unroll: Boolean,
    ctx: Context,
    acc: List[Words],
  ): (List[Words], Context) =
    val (nInsts, ctxWithN) = ExpressionCompiler.compileBlock(n.tree, ctx)

    // Hoist loop-invariant expressions before the loop
    // Only hoist expressions that don't depend on loop variables or control flow
    val bodyExprs = collectExpressionsMap(f)
    val loopDependent = findLoopDependentExprs(bodyExprs, CurrentRepeatIndex.treeid)
    val scopeDependent = findScopeDependentExprs(bodyExprs)
    // Filter out loop-dependent and scope-dependent (When, etc.) expressions
    val invariantExprs = bodyExprs.values.filter { e =>
      !loopDependent.contains(e.treeid) && !scopeDependent.contains(e.treeid)
    }.toList.sortBy(_.treeid)  // Sort by treeid to respect definition order
    val (invariantInsts, ctxWithInvariants) = invariantExprs.foldLeft((List.empty[Words], ctxWithN)) {
      case ((instsAcc, ctxAcc), expr) =>
        val (insts, newCtx) = ExpressionCompiler.compileBlock(expr, ctxAcc)
        (instsAcc ::: insts, newCtx)
    }

    val intTy = ctxWithInvariants.valueTypeMap(Int32Tag.tag)
    val boolTy = ctxWithInvariants.valueTypeMap(GBooleanTag.tag)
    val zeroId = ctxWithInvariants.constRefs((Int32Tag, 0))
    val oneId = ctxWithInvariants.constRefs((Int32Tag, 1))
    val nId = ctxWithInvariants.exprRefs(n.tree.treeid)

    val baseId = ctxWithInvariants.nextResultId
    val preHeaderId = baseId
    val headerId = baseId + 1
    val bodyId = baseId + 2
    val continueId = baseId + 3
    val mergeId = baseId + 4
    val phiId = baseId + 5
    val cmpId = baseId + 6
    val addId = baseId + 7

    val bodyCtx = ctxWithInvariants.copy(
      nextResultId = baseId + 8,
      exprRefs = ctxWithInvariants.exprRefs + (CurrentRepeatIndex.treeid -> phiId),
    )
    val (bodyInsts, ctxAfterBody) = compileGio(f, bodyCtx)

    val preheader = List(
      Instruction(Op.OpBranch, List(ResultRef(preHeaderId))),
      Instruction(Op.OpLabel, List(ResultRef(preHeaderId))),
      Instruction(Op.OpBranch, List(ResultRef(headerId))),
    )

    val header = List(
      Instruction(Op.OpLabel, List(ResultRef(headerId))),
      Instruction(
        Op.OpPhi,
        List(ResultRef(intTy), ResultRef(phiId), ResultRef(zeroId), ResultRef(preHeaderId), ResultRef(addId), ResultRef(continueId)),
      ),
      Instruction(Op.OpSLessThan, List(ResultRef(boolTy), ResultRef(cmpId), ResultRef(phiId), ResultRef(nId))),
      Instruction(Op.OpLoopMerge, List(ResultRef(mergeId), ResultRef(continueId),
        if unroll then LoopControlMask.Unroll else LoopControlMask.MaskNone)),
      Instruction(Op.OpBranchConditional, List(ResultRef(cmpId), ResultRef(bodyId), ResultRef(mergeId))),
    )

    val bodyBlk =
      List(Instruction(Op.OpLabel, List(ResultRef(bodyId)))) :::
        bodyInsts :::
        List(Instruction(Op.OpBranch, List(ResultRef(continueId))))

    val contBlk = List(
      Instruction(Op.OpLabel, List(ResultRef(continueId))),
      Instruction(Op.OpIAdd, List(ResultRef(intTy), ResultRef(addId), ResultRef(phiId), ResultRef(oneId))),
      Instruction(Op.OpBranch, List(ResultRef(headerId))),
    )

    val mergeBlk = List(Instruction(Op.OpLabel, List(ResultRef(mergeId))))

    val finalNextId = math.max(ctxAfterBody.nextResultId, addId + 1)
    val finalCtx = ctxWithInvariants.copy(nextResultId = finalNextId)

    (acc ::: nInsts ::: invariantInsts ::: preheader ::: header ::: bodyBlk ::: contBlk ::: mergeBlk, finalCtx)

  /** Compiles foldRepeat - a loop with an accumulator that can contain barriers. */
  private def compileFoldRepeat(
    fr: FoldRepeat[?],
    ctx: Context,
    acc: List[Words],
  ): (List[Words], Context) =
    val n = fr.n
    val init = fr.init
    val body = fr.body
    val accTreeId = fr.accTreeId

    // Compile n and init
    val (nInsts, ctxWithN) = ExpressionCompiler.compileBlock(n.tree, ctx)
    val (initInsts, ctxWithInit) = ExpressionCompiler.compileBlock(init.tree, ctxWithN)

    // Hoist loop-invariant expressions before the loop
    // Only hoist expressions that don't depend on loop variables
    val bodyExprs = collectExpressionsMap(body)
    val loopDependent = findLoopDependentExprs(bodyExprs, CurrentRepeatIndex.treeid) + accTreeId
    val scopeDependent = findScopeDependentExprs(bodyExprs)
    // Filter out loop-dependent and scope-dependent (When, etc.) expressions
    val invariantExprs = bodyExprs.values.filter { e =>
      !loopDependent.contains(e.treeid) && !scopeDependent.contains(e.treeid)
    }.toList.sortBy(_.treeid)  // Sort by treeid to respect definition order
    val (invariantInsts, ctxWithInvariants) = invariantExprs.foldLeft((List.empty[Words], ctxWithInit)) {
      case ((instsAcc, ctxAcc), expr) =>
        val (insts, newCtx) = ExpressionCompiler.compileBlock(expr, ctxAcc)
        (instsAcc ::: insts, newCtx)
    }

    val intTy = ctxWithInvariants.valueTypeMap(Int32Tag.tag)
    val accTy = ctxWithInvariants.valueTypeMap(init.tree.tag.tag)
    val boolTy = ctxWithInvariants.valueTypeMap(GBooleanTag.tag)
    val zeroId = ctxWithInvariants.constRefs((Int32Tag, 0))
    val oneId = ctxWithInvariants.constRefs((Int32Tag, 1))
    val nId = ctxWithInvariants.exprRefs(n.tree.treeid)
    val initId = ctxWithInvariants.exprRefs(init.tree.treeid)

    val baseId = ctxWithInvariants.nextResultId
    val preHeaderId = baseId
    val headerId = baseId + 1
    val bodyId = baseId + 2
    val continueId = baseId + 3
    val mergeId = baseId + 4
    val iterPhiId = baseId + 5    // loop counter phi
    val accPhiId = baseId + 6     // accumulator phi
    val cmpId = baseId + 7
    val addId = baseId + 8

    // Setup context for body compilation with both loop counter and accumulator
    val bodyCtx = ctxWithInvariants.copy(
      nextResultId = baseId + 9,
      exprRefs = ctxWithInvariants.exprRefs
        + (CurrentRepeatIndex.treeid -> iterPhiId)
        + (accTreeId -> accPhiId),
    )

    val (bodyInsts, ctxAfterBody) = compileGio(body, bodyCtx)
    val bodyResultId = ctxAfterBody.exprRefs(body.underlying.tree.treeid)

    val preheader = List(
      Instruction(Op.OpBranch, List(ResultRef(preHeaderId))),
      Instruction(Op.OpLabel, List(ResultRef(preHeaderId))),
      Instruction(Op.OpBranch, List(ResultRef(headerId))),
    )

    val header = List(
      Instruction(Op.OpLabel, List(ResultRef(headerId))),
      // Phi for loop counter
      Instruction(
        Op.OpPhi,
        List(ResultRef(intTy), ResultRef(iterPhiId), ResultRef(zeroId), ResultRef(preHeaderId), ResultRef(addId), ResultRef(continueId)),
      ),
      // Phi for accumulator
      Instruction(
        Op.OpPhi,
        List(ResultRef(accTy), ResultRef(accPhiId), ResultRef(initId), ResultRef(preHeaderId), ResultRef(bodyResultId), ResultRef(continueId)),
      ),
      Instruction(Op.OpSLessThan, List(ResultRef(boolTy), ResultRef(cmpId), ResultRef(iterPhiId), ResultRef(nId))),
      Instruction(Op.OpLoopMerge, List(ResultRef(mergeId), ResultRef(continueId),
        if fr.unroll then LoopControlMask.Unroll else LoopControlMask.MaskNone)),
      Instruction(Op.OpBranchConditional, List(ResultRef(cmpId), ResultRef(bodyId), ResultRef(mergeId))),
    )

    val bodyBlk =
      List(Instruction(Op.OpLabel, List(ResultRef(bodyId)))) :::
        bodyInsts :::
        List(Instruction(Op.OpBranch, List(ResultRef(continueId))))

    val contBlk = List(
      Instruction(Op.OpLabel, List(ResultRef(continueId))),
      Instruction(Op.OpIAdd, List(ResultRef(intTy), ResultRef(addId), ResultRef(iterPhiId), ResultRef(oneId))),
      Instruction(Op.OpBranch, List(ResultRef(headerId))),
    )

    val mergeBlk = List(Instruction(Op.OpLabel, List(ResultRef(mergeId))))

    val finalNextId = math.max(ctxAfterBody.nextResultId, addId + 1)
    // The result of foldRepeat is the final accumulator value (accPhiId after merge)
    // We need to map both:
    // 1. The accumulator phantom treeid (for expressions that reference the accumulator)
    // 2. The body result treeid (for the FlatMap chain to work correctly)
    val finalCtx = ctxAfterBody.copy(
      nextResultId = finalNextId,
      exprRefs = ctxAfterBody.exprRefs
        + (accTreeId -> accPhiId)
        + (body.underlying.tree.treeid -> accPhiId),
    )

    (acc ::: nInsts ::: initInsts ::: invariantInsts ::: preheader ::: header ::: bodyBlk ::: contBlk ::: mergeBlk, finalCtx)

  /** Compiles ConditionalWhen - a proper if-then construct for conditional execution.
    * Generates OpSelectionMerge + OpBranchConditional instead of a loop.
    */
  private def compileConditionalWhen(
    cond: io.computenode.cyfra.dsl.GBoolean,
    body: GIO[?],
    ctx: Context,
    acc: List[Words],
  ): (List[Words], Context) =
    // Compile the condition
    val (condInsts, ctxWithCond) = ExpressionCompiler.compileBlock(cond.tree, ctx)
    val condId = ctxWithCond.exprRefs(cond.tree.treeid)

    val baseId = ctxWithCond.nextResultId
    val headerLabelId = baseId
    val thenLabelId = baseId + 1
    val mergeLabelId = baseId + 2

    // Setup context for body compilation
    val bodyCtx = ctxWithCond.copy(nextResultId = baseId + 3)
    val (bodyInsts, ctxAfterBody) = compileGio(body, bodyCtx)

    // Header block: branch to header label, then do selection
    val headerBlock = List(
      Instruction(Op.OpBranch, List(ResultRef(headerLabelId))),
      Instruction(Op.OpLabel, List(ResultRef(headerLabelId))),
      Instruction(Op.OpSelectionMerge, List(ResultRef(mergeLabelId), SelectionControlMask.MaskNone)),
      Instruction(Op.OpBranchConditional, List(ResultRef(condId), ResultRef(thenLabelId), ResultRef(mergeLabelId))),
    )

    // Then block: execute body, then branch to merge
    val thenBlock = List(Instruction(Op.OpLabel, List(ResultRef(thenLabelId)))) :::
      bodyInsts :::
      List(Instruction(Op.OpBranch, List(ResultRef(mergeLabelId))))

    // Merge block: continuation point
    val mergeBlock = List(Instruction(Op.OpLabel, List(ResultRef(mergeLabelId))))

    val finalNextId = math.max(ctxAfterBody.nextResultId, mergeLabelId + 1)
    // Use ctxWithCond's exprRefs but updated nextResultId - same pattern as compileRepeat
    val finalCtx = ctxWithCond.copy(nextResultId = finalNextId)

    (acc ::: condInsts ::: headerBlock ::: thenBlock ::: mergeBlock, finalCtx)

  /** Finds the CurrentFoldRepeatAcc phantom expression in a GIO tree. */
  private def findFoldRepeatAcc(gio: GIO[?]): Option[CurrentFoldRepeatAcc[?]] =
    def findInExpr(expr: E[?]): Option[CurrentFoldRepeatAcc[?]] =
      expr match
        case acc: CurrentFoldRepeatAcc[?] => Some(acc)
        case _ => expr.exprDependencies.flatMap(findInExpr).headOption

    def findInGio(g: GIO[?]): Option[CurrentFoldRepeatAcc[?]] = g match
      case Pure(v)                     => findInExpr(v.tree)
      case FlatMap(v, n)               => findInGio(v).orElse(findInGio(n))
      case Repeat(n, body, _)          => findInExpr(n.tree).orElse(findInGio(body))
      case FoldRepeat(n, init, b, _, _) => findInExpr(n.tree).orElse(findInExpr(init.tree)).orElse(findInGio(b))
      case ConditionalWhen(cond, body) => findInExpr(cond.tree).orElse(findInGio(body))
      case WriteBuffer(_, i, v)        => findInExpr(i.tree).orElse(findInExpr(v.tree))
      case WriteShared(_, i, v)        => findInExpr(i.tree).orElse(findInExpr(v.tree))
      case Printf(_, args*)            => args.flatMap(a => findInExpr(a.tree)).headOption
      case WorkgroupBarrier            => None

    findInGio(gio)

  private def collectExpressionsMap(gio: GIO[?]): Map[Int, E[?]] =
    val result = mutable.Map[Int, E[?]]()

    def collectFromExpr(expr: E[?]): Unit =
      if !result.contains(expr.treeid) then
        result += (expr.treeid -> expr)
        expr.exprDependencies.foreach(collectFromExpr)

    def collectFromGio(g: GIO[?]): Unit = g match
      case Pure(v)                          => collectFromExpr(v.tree)
      case FlatMap(v, n)                    => collectFromGio(v); collectFromGio(n)
      case Repeat(n, body, _)               => collectFromExpr(n.tree); collectFromGio(body)
      case FoldRepeat(n, init, body, _, _)  => collectFromExpr(n.tree); collectFromExpr(init.tree); collectFromGio(body)
      case ConditionalWhen(cond, body)      => collectFromExpr(cond.tree); collectFromGio(body)
      case WriteBuffer(_, i, v)             => collectFromExpr(i.tree); collectFromExpr(v.tree)
      case WriteShared(_, i, v)             => collectFromExpr(i.tree); collectFromExpr(v.tree)
      case Printf(_, args*)                 => args.foreach(a => collectFromExpr(a.tree))
      case WorkgroupBarrier                 => () // No expressions to collect

    collectFromGio(gio)
    result.toMap

  private def findLoopDependentExprs(exprsMap: Map[Int, E[?]], loopVarId: Int): Set[Int] =
    val dependent = mutable.Set[Int](loopVarId)
    var changed = true
    while changed do
      changed = false
      exprsMap.values.foreach: expr =>
        if !dependent.contains(expr.treeid) then
          // Check if any dependency's treeid is in dependent set
          if expr.exprDependencies.exists(dep => dependent.contains(dep.treeid)) then
            dependent += expr.treeid
            changed = true
    dependent.toSet

  /** Find expressions that depend (transitively) on scope-introducing expressions like When. */
  private def findScopeDependentExprs(exprsMap: Map[Int, E[?]]): Set[Int] =
    val scopeExprs = exprsMap.values.filter(_.introducedScopes.nonEmpty).map(_.treeid).toSet
    val dependent = mutable.Set.from(scopeExprs)
    var changed = true
    while changed do
      changed = false
      exprsMap.values.foreach: expr =>
        if !dependent.contains(expr.treeid) then
          if expr.exprDependencies.exists(dep => dependent.contains(dep.treeid)) then
            dependent += expr.treeid
            changed = true
    dependent.toSet
