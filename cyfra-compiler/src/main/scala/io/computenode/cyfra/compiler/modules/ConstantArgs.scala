package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.Spirv.{IntWord, Words}
import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.Ctx
import io.computenode.cyfra.core.expression.{Value, given}
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.Tag

class ConstantArgs extends FunctionCompilationModule:
  def compileFunction(input: IRs[?])(using Ctx): IRs[?] =
    input.flatMapReplace:
      case x: IR.ConstantArgs =>
        IRs.proxy(x)(using x.v)
      // Only rebuild instructions that actually inline literals: a rebuilt IR gets a new id, and references from other functions
      // (e.g. OpFunctionCall -> OpFunction) are not remapped
      case x: IR.SvRef[a] if hasConstantArgs(x.operands) =>
        given Value[a] = x.v
        IRs(x.copy(operands = inlineArgs(x.operands)))
      case x: IR.SvInst if hasConstantArgs(x.operands) =>
        IRs(x.copy(operands = inlineArgs(x.operands)))
      case other => IRs(other)(using other.v)

  private def hasConstantArgs(operands: List[Words | IR.RefIR[?]]): Boolean =
    operands.exists(_.isInstanceOf[IR.ConstantArgs])

  private def inlineArgs(operands: List[Words | IR.RefIR[?]]): List[Words | IR.RefIR[?]] =
    operands.flatMap:
      case IR.ConstantArgs(args) => args.map(IntWord.apply)
      case x                     => List(x)
