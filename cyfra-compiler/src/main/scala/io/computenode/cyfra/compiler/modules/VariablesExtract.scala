package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.compiler.modules.CompilationModule.{FunctionCompilationModule, StandardCompilationModule}
import io.computenode.cyfra.compiler.unit.{Compilation, Ctx}
import io.computenode.cyfra.compiler.Compiler.Compute
import io.computenode.cyfra.core.memory.{BuildInVariable, FocusRoot, GBinding, GlobalVariable, LocalVariable, Variable}

import scala.collection.mutable

class VariablesExtract extends StandardCompilationModule:
  def compile(input: Compilation): Compilation =
    val ((newFunctions, globalVariables), context) = Ctx.withCapability(input.context):
      val (a, b) = input.functionBodies.map(moveLocalVariables).unzip
      (a, b.flatten)
    val shaderInput: Seq[FocusRoot[?]] = input.metadata.config match
      case Compute(bindings, _) => bindings
    val shaderInputIR = shaderInput.map:
      case x: FocusRoot[a] => IR.Declare(x, None)(using x.v)
    val c1 = context.copy(suffix = context.suffix ++ globalVariables ++ shaderInputIR)
    input.copy(context = c1, functionBodies = newFunctions)

  private def moveLocalVariables(input: IRs[?])(using Ctx): (IRs[?], Seq[IR[?]]) =
    val localDeclarations = mutable.Buffer.empty[IR.Declare[?]]
    val globalDeclarations = mutable.Buffer.empty[IR.Declare[?]]
    val buildInRoots = mutable.Set.empty[BuildInVariable[?]]

    val IRs(res, body) = input.flatMapReplace:
      case x @ IR.Declare(variable, init) =>
        variable match
          case _: LocalVariable[?]  => localDeclarations.append(x)
          case _: GlobalVariable[?] => globalDeclarations.append(x)
        IRs.proxy[Unit](x)
      case x @ IR.Read(variable: BuildInVariable[?], _) =>
        buildInRoots.add(variable)
        IRs(x)(using x.v)
      case x @ IR.Write(variable: BuildInVariable[?], _, _) =>
        buildInRoots.add(variable)
        IRs(x)(using x.v)
      case other => IRs(other)(using other.v)

    val buildInDeclarations = buildInRoots
      .map:
        case root: BuildInVariable[a] =>
          IR.Declare(root, None)(using root.v)
      .toSeq

    (IRs(res, localDeclarations.toList ++ body)(using res.v), globalDeclarations.toSeq ++ buildInDeclarations)
