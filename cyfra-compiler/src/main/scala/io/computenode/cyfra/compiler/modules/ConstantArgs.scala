package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.Spirv.IntWord
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
      case x: IR.SvRef[a] =>
        given Value[a] = x.v
        val newOperands = x.operands.flatMap:
          case IR.ConstantArgs(args) => args.map(IntWord.apply)
          case x                     => List(x)
        IRs(x.copy(operands = newOperands))
      case x: IR.SvInst =>
        val newOperands = x.operands.flatMap:
          case IR.ConstantArgs(args) => args.map(IntWord.apply)
          case x                     => List(x)
        IRs(x.copy(operands = newOperands))
      case other => IRs(other)(using other.v)
