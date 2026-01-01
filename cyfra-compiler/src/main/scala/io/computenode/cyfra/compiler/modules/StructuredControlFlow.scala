package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.FunctionIR
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.ir.IR.*
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.{Context, TypeManager}
import io.computenode.cyfra.compiler.unit.Ctx
import io.computenode.cyfra.compiler.spirv.Opcodes.*
import io.computenode.cyfra.core.expression.{JumpTarget, Value, given}
import io.computenode.cyfra.utility.FlatList
import izumi.reflect.Tag

import scala.collection.mutable

class StructuredControlFlow extends FunctionCompilationModule:
  override def compileFunction(input: IRs[?])(using Ctx): IRs[?] =
    val startLabel = SvRef[Unit](Op.OpLabel, Nil)
    val starter = input.prepend(startLabel)
    compileRec(starter, startLabel, mutable.Map.empty, mutable.Map.empty)._1.flatMapReplace(x => IRs(x)(using x.v))

  private def compileRec(
    irs: IRs[?],
    startingLabel: RefIR[Unit],
    targets: mutable.Map[JumpTarget[?], RefIR[?]],
    phiMap: mutable.Map[JumpTarget[?], mutable.Buffer[(RefIR[?], RefIR[?])]],
  )(using Ctx): (IRs[?], RefIR[Unit]) =
    var currentLabel = startingLabel
    val res = irs.flatMapReplace(enterControlFlow = false):
      case x: Branch[a] =>
        given v: Value[a] = x.v
        val Branch(cond, ifTrue, ifFalse, break) = x
        val trueLabel = SvRef[Unit](Op.OpLabel, Nil)
        val falseLabel = SvRef[Unit](Op.OpLabel, Nil)
        val mergeLabel = SvRef[Unit](Op.OpLabel, Nil)

        targets(break) = mergeLabel
        phiMap(break) = mutable.Buffer.empty

        val (IRs(trueRes, trueBody), afterTrueLabel) = compileRec(ifTrue, trueLabel, targets, phiMap)
        val (IRs(falseRes, falseBody), afterFalseLabel) = compileRec(ifFalse, falseLabel, targets, phiMap)

        phiMap(break).append((trueRes.asInstanceOf[RefIR[?]], afterTrueLabel))
        phiMap(break).append((falseRes.asInstanceOf[RefIR[?]], afterFalseLabel))

        val ifBlock: List[IR[?]] = FlatList(
          SvInst(Op.OpSelectionMerge, List(mergeLabel, SelectionControlMask.MaskNone)),
          SvInst(Op.OpBranchConditional, List(cond, trueLabel, falseLabel)),
          trueLabel,
          trueBody,
          SvInst(Op.OpBranch, List(mergeLabel)),
          falseLabel,
          falseBody,
          SvInst(Op.OpBranch, List(mergeLabel)),
          mergeLabel,
        )

        currentLabel = mergeLabel

        if v.tag =:= Tag[Unit] then IRs[Unit](mergeLabel, ifBlock)
        else
          val phiJumps: List[RefIR[?]] = phiMap(break).toList.flatMap(x => List(x._1, x._2))
          val phi = SvRef[a](Op.OpPhi, Ctx.getType(v) , phiJumps)
          IRs[a](phi, ifBlock.appended(phi))

      case Loop(mainBody, continueBody, break, continue) =>
        val loopLabel = SvRef[Unit](Op.OpLabel, Nil)
        val bodyLabel = SvRef[Unit](Op.OpLabel, Nil)
        val continueLabel = SvRef[Unit](Op.OpLabel, Nil)
        val mergeLabel = SvRef[Unit](Op.OpLabel, Nil)

        targets(break) = mergeLabel
        targets(continue) = continueLabel
        phiMap(break) = mutable.Buffer.empty
        phiMap(continue) = mutable.Buffer.empty

        val body: List[IR[?]] =
          FlatList(
            loopLabel,
            SvInst(Op.OpLoopMerge, List(mergeLabel, continueLabel, LoopControlMask.MaskNone)),
            SvInst(Op.OpBranch, List(bodyLabel)),
            bodyLabel,
            compileRec(mainBody, bodyLabel, targets, phiMap)._1.body,
            SvInst(Op.OpBranch, List(continueLabel)),
            continueLabel,
            compileRec(continueBody, continueLabel, targets, phiMap)._1.body,
            SvInst(Op.OpBranch, List(loopLabel)),
            mergeLabel,
          )
        currentLabel = mergeLabel
        IRs[Unit](loopLabel, body)

      case Jump(target, value) =>
        phiMap(target).append((value, currentLabel))
        IRs[Unit](SvInst(Op.OpBranch, targets(target) :: Nil))
      case ConditionalJump(cond, target, value) =>
        phiMap(target).append((value, currentLabel))
        val followingLabel = SvRef[Unit](Op.OpLabel, Nil)

        val body: List[IR[?]] =
          SvInst(Op.OpBranchConditional, List(cond, targets(target), followingLabel)) :: followingLabel :: Nil
        currentLabel = followingLabel
        IRs[Unit](followingLabel, body)
      case other =>
        IRs(other)(using other.v)

    (res, currentLabel)
