package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.{FunctionIR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.Context

class Variables extends FunctionCompilationModule:
  override def compileFunction(input: IRs[_], context: Context): IRs[_] = ???
