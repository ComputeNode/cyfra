package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.dsl.binding.*
import io.computenode.cyfra.dsl.gio.GIO.CurrentRepeatIndex
import io.computenode.cyfra.spirv.SpirvTypes.{Int32Tag, LInt32Tag}

object GIOCompiler:
  
  def compileGio(gio: GIO[?], ctx: Context, acc: List[Words] = Nil): (List[Words], Context) =
    gio match
      
      case GIO.Pure(v) =>
        val (insts, updatedCtx) = ExpressionCompiler.compileBlock(v.tree, ctx)
        (acc ::: insts, updatedCtx)
        
      case WriteBuffer(buffer, index, value) =>
        val insns = List(Instruction(
            Op.OpAccessChain,
            List(
              ResultRef(ctx.uniformPointerMap(ctx.valueTypeMap(buffer.tag.tag))),
              ResultRef(ctx.nextResultId),
              ResultRef(ctx.bufferBlocks(buffer).blockVarRef),
              ResultRef(ctx.constRefs((Int32Tag, 0))),
              ResultRef(ctx.workerIndexRef),
            ),
          ),
          Instruction(Op.OpStore, List(ResultRef(ctx.nextResultId), ResultRef(ctx.exprRefs(value.tree.treeid))))
        )
        val updatedCtx = ctx.copy(nextResultId = ctx.nextResultId + 1)
        (acc ::: insns, updatedCtx)
        
      case GIO.Repeat(n, f) =>
        val (nInsts, ctxWithN) = ExpressionCompiler.compileBlock(n.tree, ctx)
        val loopHeaderId = ctxWithN.nextResultId
        val loopMergeId = ctxWithN.nextResultId + 1
        val loopContinueId = ctxWithN.nextResultId + 2
        val counterVarId = ctxWithN.nextResultId + 3
        val ctxWithCounter = ctxWithN.copy(exprRefs = ctxWithN.exprRefs + (CurrentRepeatIndex.treeid -> counterVarId), nextResultId = ctxWithN.nextResultId + 4)
        val counterInit = Instruction(Op.OpVariable, List(ResultRef(ctxWithCounter.constRefs((Int32Tag, 0))), ResultRef(counterVarId), StorageClass.Function))
        val counterStore = Instruction(Op.OpStore, List(ResultRef(counterVarId), ResultRef(ctxWithCounter.constRefs((Int32Tag, 0)))))
        val (body, afterBodyCtx) = compileGio(f, ctxWithCounter)
        // todo
        ???
        
        
        
        
        
        
      