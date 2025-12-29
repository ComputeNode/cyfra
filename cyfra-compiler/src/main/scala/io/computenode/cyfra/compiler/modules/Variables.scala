package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.core.expression.{Value, given}
import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.{Context, Ctx}
import io.computenode.cyfra.compiler.spirv.Opcodes.Op
import io.computenode.cyfra.compiler.spirv.Opcodes.StorageClass

import scala.collection.mutable

class Variables extends FunctionCompilationModule:
  override def compileFunction(input: IRs[?])(using Ctx): IRs[?] =
    val varDeclarations = mutable.Map.empty[Int, RefIR[Unit]]
    input.flatMapReplace:
      case IR.VarDeclare(variable) =>
        val inst = IR.SvRef[Unit](Op.OpVariable, List(Ctx.getTypePointer(variable.v, StorageClass.Function), StorageClass.Function))
        varDeclarations(variable.id) = inst
        IRs(inst)
      case IR.VarWrite(variable, value) =>
        val inst = IR.SvInst(Op.OpStore, List(varDeclarations(variable.id), value))
        IRs(inst)
      case x: IR.VarRead[a] =>
        given Value[a] = x.v
        val IR.VarRead(variable) = x
        val inst = IR.SvRef[a](Op.OpLoad, List(Ctx.getType(variable.v), varDeclarations(variable.id)))
        IRs(inst)
      case x: IR.CallWithVar[a] =>
        given v: Value[a] = x.v
        val IR.CallWithVar(func, args) = x
        val inst = IR.CallWithIR(func, args.map(arg => varDeclarations(arg.id)))
        IRs(inst)
      case other => IRs(other)(using other.v)
