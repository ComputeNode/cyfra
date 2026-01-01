package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.binding.GBinding
import io.computenode.cyfra.core.expression.ExpressionBlock
import io.computenode.cyfra.core.layout.LayoutStruct
import io.computenode.cyfra.compiler.modules.*
import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.Compilation

import java.nio.ByteBuffer

class Compiler(verbose: Boolean = false):
  private val transformer = new Transformer()
  private val modules: List[StandardCompilationModule] =
    List(new StructuredControlFlow, new Variables, new Functions, new Bindings, new Constants, new Algebra, new Finalizer)
  private val emitter = new Emitter()

  def compile(bindings: Seq[GBinding[?]], body: ExpressionBlock[Unit]): ByteBuffer =
    val parsedUnit = transformer.compile(body).copy(bindings = bindings)
    if verbose then
      println(s"=== ${transformer.name} ===")
      Compilation.debugPrint(parsedUnit)

    val compiledUnit = modules.foldLeft(parsedUnit): (unit, module) =>
      val res = module.compile(unit)
      if verbose then
        println(s"\n=== ${module.name} ===")
        Compilation.debugPrint(res)
      res

    emitter.compile(compiledUnit)
