package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.unit.Compilation

trait CompilationModule[A, B]:
  def compile(input: A): B

object CompilationModule:

  trait StandardCompilationModule extends CompilationModule[Compilation, Unit]
