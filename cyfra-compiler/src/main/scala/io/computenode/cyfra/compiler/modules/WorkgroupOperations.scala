package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.Ctx
import io.computenode.cyfra.core.expression.{Operator, Value}
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.Tag

/** Lower synchronization and subgroup operators without adding new expression or IR node families. */
class WorkgroupOperations extends FunctionCompilationModule:
  def compileFunction(input: IRs[?])(using Ctx): IRs[?] = input.flatMapReplace:
    case IR.Operation(Operator.WorkgroupBarrier, _) =>
      val scope = Ctx.getConstant[UInt32](Scope.Workgroup.opcode)
      val semantics = Ctx.getConstant[UInt32](MemorySemanticsMask.AcquireRelease.opcode | MemorySemanticsMask.WorkgroupMemory.opcode)
      IRs(IR.Constant[Unit](()), List(IR.SvInst(Op.OpControlBarrier, List(scope, scope, semantics))))
    case x: IR.Operation[a] if isSubgroup(x.func) =>
      given Value[a] = x.v
      Ctx.enableCapability(Capability.GroupNonUniform)
      val scope = Ctx.getConstant[UInt32](Scope.Subgroup.opcode)
      val tag = x.v.bottomComposite.tag
      def numeric(float: Code, signed: Code, unsigned: Code): Code =
        if tag <:< Tag[FloatType] then float
        else if tag <:< Tag[SignedIntType] then signed
        else unsigned
      val (code, capability, group) = x.func match
        case Operator.SubgroupAdd | Operator.SubgroupInclusiveAdd | Operator.SubgroupExclusiveAdd =>
          val operation = x.func match
            case Operator.SubgroupInclusiveAdd => GroupOperation.InclusiveScan
            case Operator.SubgroupExclusiveAdd => GroupOperation.ExclusiveScan
            case _                             => GroupOperation.Reduce
          (numeric(Op.OpGroupNonUniformFAdd, Op.OpGroupNonUniformIAdd, Op.OpGroupNonUniformIAdd), Capability.GroupNonUniformArithmetic, List(operation))
        case Operator.SubgroupMin =>
          (
            numeric(Op.OpGroupNonUniformFMin, Op.OpGroupNonUniformSMin, Op.OpGroupNonUniformUMin),
            Capability.GroupNonUniformArithmetic,
            List(GroupOperation.Reduce),
          )
        case Operator.SubgroupMax =>
          (
            numeric(Op.OpGroupNonUniformFMax, Op.OpGroupNonUniformSMax, Op.OpGroupNonUniformUMax),
            Capability.GroupNonUniformArithmetic,
            List(GroupOperation.Reduce),
          )
        case Operator.SubgroupBroadcast      => (Op.OpGroupNonUniformBroadcast, Capability.GroupNonUniformBallot, Nil)
        case Operator.SubgroupBroadcastFirst => (Op.OpGroupNonUniformBroadcastFirst, Capability.GroupNonUniformBallot, Nil)
        case Operator.SubgroupShuffle        => (Op.OpGroupNonUniformShuffle, Capability.GroupNonUniformShuffle, Nil)
        case Operator.SubgroupShuffleXor     => (Op.OpGroupNonUniformShuffleXor, Capability.GroupNonUniformShuffle, Nil)
        case other                           => throw IllegalArgumentException(s"Unsupported subgroup operation $other")
      Ctx.enableCapability(capability)
      IRs(IR.SvRef[a](code, Ctx.getType(x.v), scope :: group ::: x.args))
    case other => IRs(other)(using other.v)

  private def isSubgroup(op: Operator): Boolean = op match
    case Operator.SubgroupAdd | Operator.SubgroupInclusiveAdd | Operator.SubgroupExclusiveAdd | Operator.SubgroupMin | Operator.SubgroupMax |
        Operator.SubgroupBroadcast | Operator.SubgroupBroadcastFirst | Operator.SubgroupShuffle | Operator.SubgroupShuffleXor =>
      true
    case _ => false
