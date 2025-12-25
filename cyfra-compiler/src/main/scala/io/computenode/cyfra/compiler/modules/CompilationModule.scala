package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.CompilationUnit

trait CompilationModule[A, B]:
  def compile(input: A): B

object CompilationModule:

  trait StandardCompilationModule extends CompilationModule[CompilationUnit, Unit]
