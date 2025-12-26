package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.binding.GBinding
import io.computenode.cyfra.core.expression.ExpressionBlock
import io.computenode.cyfra.core.layout.LayoutStruct
import io.computenode.cyfra.compiler.modules.*
import io.computenode.cyfra.compiler.unit.Compilation

class Compiler(verbose: Boolean = false):
  def compile(bindings: Seq[GBinding[?]], body: ExpressionBlock[Unit]): Unit =
    val p = new Parser()
    val parsed = p.compile(body)
    if verbose then Compilation.debugPrint(parsed)
    ()
