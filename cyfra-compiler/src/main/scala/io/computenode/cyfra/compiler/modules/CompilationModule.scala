package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.FunctionIR
import io.computenode.cyfra.compiler.unit.{Compilation, Header}

trait CompilationModule[A, B]:
  def compile(input: A): B
  
  def name: String = this.getClass.getSimpleName.replaceAll("\\$$", "")

object CompilationModule:

  trait StandardCompilationModule extends CompilationModule[Compilation, Unit]

  trait FunctionCompilationModule extends StandardCompilationModule:
    def compileFunction(input: FunctionIR[?], header: Header): Unit

    def compile(input: Compilation): Unit =
      input.functions.foreach(compileFunction(_, input.header))
