package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.binding.GBinding
import io.computenode.cyfra.core.expression.ExpressionBlock
import io.computenode.cyfra.core.layout.LayoutStruct

class Compiler:
  def compile(bindings: List[GBinding[?]], body: ExpressionBlock[Unit]): Int =
    ???


@main
def main(): Unit =
  println("Compiler module")