package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.FunctionIR
import io.computenode.cyfra.compiler.modules.CompilationModule.{FunctionCompilationModule, StandardCompilationModule}
import io.computenode.cyfra.compiler.unit.{Compilation, Header}

class Bindings extends StandardCompilationModule:
  override def compile(input: Compilation): Unit = ()
  
