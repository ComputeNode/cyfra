package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.FunctionIR
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.ir.IR.*
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.{Context, TypeManager}
import io.computenode.cyfra.compiler.spirv.Opcodes.*
import io.computenode.cyfra.core.expression.{JumpTarget, Value, given}
import io.computenode.cyfra.utility.FlatList
import izumi.reflect.Tag

import scala.collection.mutable

class StructuredControlFlow extends FunctionCompilationModule:
  override def compileFunction(input: IRs[?], context: Context) =
    val targets: mutable.Map[JumpTarget[?], IR[?]] = mutable.Map.empty
    val phiMap: mutable.Map[JumpTarget[?], mutable.Buffer[(IR[?], IR[?])]] = mutable.Map.empty.withDefault(_ => mutable.Buffer.empty)
    compileRec(input, None, targets, phiMap, context.types)

  private def compileRec(
    irs: IRs[?],
    startingLabel: Option[IR[Unit]],
    targets: mutable.Map[JumpTarget[?], IR[?]],
    phiMap: mutable.Map[JumpTarget[?], mutable.Buffer[(IR[?], IR[?])]],
    types: TypeManager,
  ): IRs[?] =
    var currentLabel = startingLabel
    irs.flatMapReplace:
      case x: Branch[a] =>
        given v: Value[a] = x.v
        val Branch(cond, ifTrue, ifFalse, break) = x
        val trueLabel = SvInst(Op.OpLabel, Nil)
        val falseLabel = SvInst(Op.OpLabel, Nil)
        val mergeLabel = SvInst(Op.OpLabel, Nil)

        targets(break) = mergeLabel

        val ifBlock: List[IR[?]] = FlatList(
          SvInst(Op.OpSelectionMerge, List(mergeLabel, SelectionControlMask.MaskNone)),
          SvInst(Op.OpBranchConditional, List(cond, trueLabel, falseLabel)),
          trueLabel,
          compileRec(ifTrue, Some(trueLabel), targets, phiMap, types).body,
          falseLabel,
          compileRec(ifFalse, Some(falseLabel), targets, phiMap, types).body,
          mergeLabel,
        )

        currentLabel = Some(mergeLabel)

        if v.tag =:= Tag[Unit] then IRs[Unit](mergeLabel, ifBlock)
        else
          val phiJumps: List[IR[?]] = phiMap(break).toList.flatMap(x => List(x._1, x._2))
          val phi = SvInst.T[a](Op.OpPhi, types.getType(v) :: phiJumps)
          IRs[a](phi, ifBlock.appended(phi))

      case Loop(mainBody, continueBody, break, continue) =>
        val loopLabel = SvInst(Op.OpLabel, Nil)
        val bodyLabel = SvInst(Op.OpLabel, Nil)
        val continueLabel = SvInst(Op.OpLabel, Nil)
        val mergeLabel = SvInst(Op.OpLabel, Nil)

        targets(break) = mergeLabel
        targets(continue) = continueLabel

        val body: List[IR[?]] =
          FlatList(
            loopLabel,
            SvInst(Op.OpLoopMerge, List(mergeLabel, continueLabel, LoopControlMask.MaskNone)),
            SvInst(Op.OpBranch, List(bodyLabel)),
            bodyLabel,
            compileRec(mainBody, Some(bodyLabel), targets, phiMap, types).body,
            SvInst(Op.OpBranch, List(continueLabel)),
            continueLabel,
            compileRec(continueBody, Some(continueLabel), targets, phiMap, types).body,
            SvInst(Op.OpBranch, List(loopLabel)),
            mergeLabel,
          )
        currentLabel = Some(mergeLabel)
        IRs[Unit](loopLabel, body)

      case Jump(target, value) =>
        phiMap(target).append((value, currentLabel.get))
        IRs[Unit](SvInst(Op.OpBranch, targets(target) :: Nil))
      case ConditionalJump(cond, target, value) =>
        phiMap(target).append((value, currentLabel.get))
        val followingLabel = SvInst(Op.OpLabel, Nil)

        val body: List[IR[?]] =
          SvInst(Op.OpBranchConditional, List(cond, targets(target), followingLabel)) :: followingLabel :: Nil
        currentLabel = Some(followingLabel)
        IRs[Unit](followingLabel, body)
      case other => IRs(other)(using other.v)
