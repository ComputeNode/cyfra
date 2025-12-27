package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.{FunctionIR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.Context

class Algebra extends FunctionCompilationModule:

  def compileFunction(input: IRs[?], context: Context): IRs[?] = ???

  