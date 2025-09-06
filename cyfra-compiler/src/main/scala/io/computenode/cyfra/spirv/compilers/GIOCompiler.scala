package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.dsl.binding.*
import io.computenode.cyfra.dsl.gio.GIO.CurrentRepeatIndex
import io.computenode.cyfra.spirv.SpirvTypes.{GBooleanTag, Int32Tag, LInt32Tag}

object GIOCompiler:
  
  def compileGio(gio: GIO[?], ctx: Context, acc: List[Words] = Nil): (List[Words], Context) =
    gio match

      case GIO.Pure(v) =>
        val (insts, updatedCtx) = ExpressionCompiler.compileBlock(v.tree, ctx)
        (acc ::: insts, updatedCtx)
        
      case WriteBuffer(buffer, index, value) =>
        val (valueInsts, ctxWithValue) = ExpressionCompiler.compileBlock(value.tree, ctx)
        val (indexInsts, ctxWithIndex) = ExpressionCompiler.compileBlock(index.tree, ctxWithValue)

        val insns = List(Instruction(
            Op.OpAccessChain,
            List(
              ResultRef(ctxWithIndex.uniformPointerMap(ctxWithIndex.valueTypeMap(buffer.tag.tag))),
              ResultRef(ctxWithIndex.nextResultId),
              ResultRef(ctxWithIndex.bufferBlocks(buffer).blockVarRef),
              ResultRef(ctxWithIndex.constRefs((Int32Tag, 0))),
              ResultRef(ctxWithIndex.exprRefs(index.tree.treeid)),
            ),
          ),
          Instruction(Op.OpStore, List(ResultRef(ctxWithIndex.nextResultId), ResultRef(ctxWithIndex.exprRefs(value.tree.treeid))))
        )
        val updatedCtx = ctxWithIndex.copy(nextResultId = ctxWithIndex.nextResultId + 1)
        (acc ::: indexInsts ::: valueInsts ::: insns, updatedCtx)

      case GIO.FlatMap(v, n) =>
        val (vInsts, ctxAfterV) = compileGio(v, ctx, acc)
        compileGio(n, ctxAfterV, vInsts)

      case GIO.Repeat(n, f) =>

        // Compile 'n' first (so we can use its id in the comparison)
        val (nInsts, ctxWithN) = ExpressionCompiler.compileBlock(n.tree, ctx)

        // Types and constants
        val intTy = ctxWithN.valueTypeMap(Int32Tag.tag)
        val boolTy = ctxWithN.valueTypeMap(GBooleanTag.tag)
        val zeroId = ctxWithN.constRefs((Int32Tag, 0))
        val oneId = ctxWithN.constRefs((Int32Tag, 1))
        val nId = ctxWithN.exprRefs(n.tree.treeid)

        // Reserve ids for blocks and results
        val baseId = ctxWithN.nextResultId
        val preHeaderId = baseId
        val headerId = baseId + 1
        val bodyId = baseId + 2
        val continueId = baseId + 3
        val mergeId = baseId + 4
        val phiId = baseId + 5
        val cmpId = baseId + 6
        val addId = baseId + 7

        // Bind CurrentRepeatIndex to the phi result for body compilation
        val bodyCtx = ctxWithN.copy(
          nextResultId = baseId + 8,
          exprRefs = ctxWithN.exprRefs + (CurrentRepeatIndex.treeid -> phiId)
        )
        val (bodyInsts, ctxAfterBody) = compileGio(f, bodyCtx)

        // Preheader: close current block and jump to header through a dedicated block
        val preheader = List(
          Instruction(Op.OpBranch, List(ResultRef(preHeaderId))),
          Instruction(Op.OpLabel, List(ResultRef(preHeaderId))),
          Instruction(Op.OpBranch, List(ResultRef(headerId)))
        )

        // Header: OpPhi first, then compute condition, then OpLoopMerge and the terminating branch
        val header = List(
          Instruction(Op.OpLabel, List(ResultRef(headerId))),
          // OpPhi must be first in the block
          Instruction(
            Op.OpPhi,
            List(
              ResultRef(intTy), ResultRef(phiId),
              ResultRef(zeroId), ResultRef(preHeaderId),
              ResultRef(addId), ResultRef(continueId)
            )
          ),
          // cmp = (counter < n)
          Instruction(
            Op.OpSLessThan,
            List(ResultRef(boolTy), ResultRef(cmpId), ResultRef(phiId), ResultRef(nId))
          ),
          // OpLoopMerge must be the second-to-last instruction, before the terminating branch
          Instruction(Op.OpLoopMerge, List(ResultRef(mergeId), ResultRef(continueId), LoopControlMask.MaskNone)),
          Instruction(Op.OpBranchConditional, List(ResultRef(cmpId), ResultRef(bodyId), ResultRef(mergeId)))
        )

        val bodyBlk = List(
          Instruction(Op.OpLabel, List(ResultRef(bodyId)))
        ) ::: bodyInsts ::: List(
          Instruction(Op.OpBranch, List(ResultRef(continueId)))
        )

        val contBlk = List(
          Instruction(Op.OpLabel, List(ResultRef(continueId))),
          Instruction(
            Op.OpIAdd,
            List(ResultRef(intTy), ResultRef(addId), ResultRef(phiId), ResultRef(oneId))
          ),
          Instruction(Op.OpBranch, List(ResultRef(headerId)))
        )

        val mergeBlk = List(
          Instruction(Op.OpLabel, List(ResultRef(mergeId)))
        )

        val finalNextId = math.max(ctxAfterBody.nextResultId, addId + 1)
        val finalCtx = ctxAfterBody.copy(
          nextResultId = finalNextId,
          exprRefs = ctxAfterBody.exprRefs - CurrentRepeatIndex.treeid
        )

        (acc ::: nInsts ::: preheader ::: header ::: bodyBlk ::: contBlk ::: mergeBlk, finalCtx)




        
        
      