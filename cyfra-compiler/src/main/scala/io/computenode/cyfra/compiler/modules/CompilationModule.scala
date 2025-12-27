package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.{FunctionIR, IRs}
import io.computenode.cyfra.compiler.unit.{Compilation, Context}

trait CompilationModule[A, B]:
  def compile(input: A): B

  def name: String = this.getClass.getSimpleName.replaceAll("\\$$", "")

object CompilationModule:

  trait StandardCompilationModule extends CompilationModule[Compilation, Compilation]

  trait FunctionCompilationModule extends StandardCompilationModule:
    def compileFunction(input: IRs[?], context: Context): IRs[?]

    def compile(input: Compilation): Compilation =
      val newFunctions = input.functionBodies.map(x => compileFunction(x, input.context))
      input.copy(functionBodies = newFunctions) 
