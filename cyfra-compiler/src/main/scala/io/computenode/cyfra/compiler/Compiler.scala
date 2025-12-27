package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.binding.GBinding
import io.computenode.cyfra.core.expression.ExpressionBlock
import io.computenode.cyfra.core.layout.LayoutStruct
import io.computenode.cyfra.compiler.modules.*
import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.Compilation

class Compiler(verbose: Boolean = false):
  private val parser = new Parser()
  private val modules: List[StandardCompilationModule] = List(
    new StructuredControlFlow,
//    new Variables,
//    new Bindings,
//    new Functions,
//    new Algebra
  )
  private val emitter = new Emitter()

  def compile(bindings: Seq[GBinding[?]], body: ExpressionBlock[Unit]): Unit =
    val unit = parser.compile(body)
    if verbose then 
      println(s"=== ${parser.name} ===")
      Compilation.debugPrint(unit)

    modules.foreach: module =>
      module.compile(unit)
      if verbose then 
        println(s"\n=== ${module.name} ===")
        Compilation.debugPrint(unit)

    emitter.compile(unit)
