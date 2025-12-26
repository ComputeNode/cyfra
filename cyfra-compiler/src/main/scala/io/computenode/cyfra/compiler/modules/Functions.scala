package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.FunctionIR
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.Header

class Functions extends FunctionCompilationModule:
  override def compileFunction(input: FunctionIR[?], header: Header): Unit =
    ()
