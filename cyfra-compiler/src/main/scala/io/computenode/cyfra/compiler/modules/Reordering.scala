package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.Ctx

import scala.collection.mutable

class Reordering extends FunctionCompilationModule:
  def compileFunction(input: IRs[?])(using Ctx): IRs[?] =
    val declarations = mutable.Buffer[IR.VarDeclare[?]]()

    val IRs(res, body) = input.flatMapReplace:
      case x @ IR.VarDeclare(variable) =>
        declarations.append(x)
        IRs.proxy[Unit](x)
      case other => IRs(other)(using other.v)

    IRs(res, declarations.toList ++ body)(using res.v)
