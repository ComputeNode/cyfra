package io.computenode.cyfra.spirv.compilers

import ExpressionCompiler.compileBlock
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.dsl.Control.WhenExpr
import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.spirv.{Context, BlockBuilder}
import izumi.reflect.Tag
import io.computenode.cyfra.spirv.SpirvConstants.*
import io.computenode.cyfra.spirv.SpirvTypes.*

private[cyfra] object WhenCompiler:
  
  def compileWhen(when: WhenExpr[_], ctx: Context): (List[Words], Context) = {
    def compileCases(
      ctx: Context,
      resultVar: Int,
      conditions: List[E[_]],
      thenCodes: List[E[_]],
      elseCode: E[_]
    ): (List[Words], Context) = (conditions, thenCodes) match {
      case (Nil, Nil) =>
        val (elseInstructions, elseCtx) = compileBlock(
          elseCode,
          ctx
        )
        val elseWithStore = elseInstructions :+ Instruction(Op.OpStore, List(
          ResultRef(resultVar),
          ResultRef(elseCtx.exprRefs(elseCode.treeid))
        ))
        (elseWithStore, elseCtx)
      case (caseWhen :: cTail, tCode :: tTail) =>
        val (whenInstructions, whenCtx) = compileBlock(
          caseWhen,
          ctx
        )
        val (thenInstructions, thenCtx) = compileBlock(
          tCode,
          whenCtx
        )
        val thenWithStore = thenInstructions :+ Instruction(Op.OpStore, List(
          ResultRef(resultVar),
          ResultRef(thenCtx.exprRefs(tCode.treeid))
        ))
        val postCtx = whenCtx.joinNested(thenCtx)
        val endIfLabel = postCtx.nextResultId
        val thenLabel = postCtx.nextResultId + 1
        val elseLabel = postCtx.nextResultId + 2
        val contextForNextIter = postCtx.copy(nextResultId = postCtx.nextResultId + 3)
        val (elseInstructions, elseCtx) = compileCases(
          contextForNextIter,
          resultVar,
          cTail,
          tTail,
          elseCode
        )
        (
          whenInstructions ::: List(
            Instruction(Op.OpSelectionMerge, List(
              ResultRef(endIfLabel),
              SelectionControlMask.MaskNone
            )),
            Instruction(Op.OpBranchConditional, List(
              ResultRef(postCtx.exprRefs(caseWhen.treeid)),
              ResultRef(thenLabel),
              ResultRef(elseLabel)
            )),
            Instruction(Op.OpLabel, List(ResultRef(thenLabel))) // then
          ) ::: thenWithStore ::: List(
            Instruction(Op.OpBranch, List(ResultRef(endIfLabel))),
            Instruction(Op.OpLabel, List(ResultRef(elseLabel))) // else
          ) ::: elseInstructions ::: List(
            Instruction(Op.OpBranch, List(ResultRef(endIfLabel))),
            Instruction(Op.OpLabel, List(ResultRef(endIfLabel))) // end
          ),
          postCtx.joinNested(elseCtx)
        )
    }
  
    val resultVar = ctx.nextResultId
    val resultLoaded = ctx.nextResultId + 1
    val resultTypeTag = ctx.valueTypeMap(when.tag.tag)
    val contextForCases = ctx.copy(nextResultId = ctx.nextResultId + 2)
  
    val blockDeps = when.introducedScopes
    val thenCode = blockDeps.head.expr
    val elseCode = blockDeps.last.expr
    val (conds, thenCodes) = blockDeps.map(_.expr).tail.init.splitAt(when.otherConds.length)
    val (caseInstructions, caseCtx) = compileCases(
      contextForCases,
      resultVar,
      when.exprDependencies.head :: conds,
      thenCode :: thenCodes,
      elseCode
    )
    val instructions = List(
      Instruction(Op.OpVariable, List(
        ResultRef(ctx.funPointerTypeMap(resultTypeTag)),
        ResultRef(resultVar),
        StorageClass.Function
      ))) ::: caseInstructions ::: List(
      Instruction(Op.OpLoad, List(
        ResultRef(resultTypeTag),
        ResultRef(resultLoaded),
        ResultRef(resultVar)
      ))
    )
    (instructions, caseCtx.copy(exprRefs = caseCtx.exprRefs + (when.treeid -> resultLoaded)))
  }