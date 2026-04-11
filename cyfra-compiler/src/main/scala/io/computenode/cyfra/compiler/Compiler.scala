package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.memory.GBinding
import io.computenode.cyfra.core.expression.ExpressionBlock
import io.computenode.cyfra.compiler.modules.*
import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.core.GProgram.WorkDimensions

import java.nio.ByteBuffer

class Compiler(verbose: "none" | "last" | "all" = "none"):
  private val transformer = new Transformer()
  private val modules: List[StandardCompilationModule] =
    List(
      new VariablesExtract,
      new VariablesDigestion,
      new StructuredControlFlow,
      new Functions,
      new CompositeManipulator,
      new Constants,
      new Algebra,
      new ConstantArgs,
      new Finalizer,
    )
  private val emitter = new Emitter()

  def compile(body: ExpressionBlock[Unit], config: Compiler.Config): ByteBuffer =
    val parsed = transformer.compile((body, config))
    if verbose == "all" then
      println(s"=== ${transformer.name} ===")
      Compilation.debugPrint(parsed)

    val compiledUnit = modules.foldLeft(parsed): (unit, module) =>
      val res = module.compile(unit)
      if verbose == "all" then
        println(s"\n=== ${module.name} ===")
        Compilation.debugPrint(res)
      res

    if verbose == "last" then
      println(s"\n=== Final Output ===")
      Compilation.debugPrint(compiledUnit)

    emitter.compile(compiledUnit)

object Compiler:
  sealed trait Config
  case class Compute(bindings: Seq[GBinding[?]], workgroupSize: WorkDimensions) extends Config
