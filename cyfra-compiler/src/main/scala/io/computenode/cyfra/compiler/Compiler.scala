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
    new Variables,
    new Functions,
//    new Bindings,
//    new Functions,
//    new Algebra
  )
  private val emitter = new Emitter()

  def compile(bindings: Seq[GBinding[?]], body: ExpressionBlock[Unit]): Unit =
    val parsedUnit = parser.compile(body)
    if verbose then
      println(s"=== ${parser.name} ===")
      Compilation.debugPrint(parsedUnit)

    val compiledUnit = modules.foldLeft(parsedUnit): (unit, module) =>
      val res = module.compile(unit)
      if verbose then
        println(s"\n=== ${module.name} ===")
        Compilation.debugPrint(res)
      res

    emitter.compile(compiledUnit)
