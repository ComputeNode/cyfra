package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.binding.GBinding
import io.computenode.cyfra.core.expression.ExpressionBlock
import io.computenode.cyfra.compiler.modules.*
import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.core.GProgram.WorkDimensions

import java.nio.ByteBuffer

class Compiler(verbose: "none" | "last" | "all" = "none"):
  private val transformer = new Transformer()
  private val modules: List[StandardCompilationModule] =
    List(new Reordering, new StructuredControlFlow, new Variables, new Functions, new Bindings, new Constants, new Algebra, new Finalizer)
  private val emitter = new Emitter()

  def compile(bindings: Seq[GBinding[?]], body: ExpressionBlock[Unit], workgroupSize: WorkDimensions): ByteBuffer =
    val parsedUnit =
      val tmp = transformer.compile(body)
      val meta = tmp.metadata.copy(bindings = bindings, workgroupSize = workgroupSize)
      tmp.copy(metadata = meta)
    if verbose == "all" then
      println(s"=== ${transformer.name} ===")
      Compilation.debugPrint(parsedUnit)

    val compiledUnit = modules.foldLeft(parsedUnit): (unit, module) =>
      val res = module.compile(unit)
      if verbose == "all" then
        println(s"\n=== ${module.name} ===")
        Compilation.debugPrint(res)
      res

    if verbose == "last" then
      println(s"\n=== Final Output ===")
      Compilation.debugPrint(compiledUnit)

    emitter.compile(compiledUnit)
