package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.{FunctionIR, IRs}
import io.computenode.cyfra.compiler.unit.{Compilation, Ctx}

trait CompilationModule[A, B]:
  def compile(input: A): B

  def name: String = this.getClass.getSimpleName.replace("$", "")

object CompilationModule:

  trait StandardCompilationModule extends CompilationModule[Compilation, Compilation]

  trait FunctionCompilationModule extends StandardCompilationModule:
    def compileFunction(input: IRs[?])(using Ctx): IRs[?]

    def compile(input: Compilation): Compilation =
      val (newFunctions, context) = Ctx.withCapability(input.context):
        input.functionBodies.map(compileFunction)
      input.copy(context = context, functionBodies = newFunctions)
