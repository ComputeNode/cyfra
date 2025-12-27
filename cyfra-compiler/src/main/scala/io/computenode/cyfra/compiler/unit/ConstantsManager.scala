package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.core.expression.Value

case class ConstantsManager(block: List[IR[?]] = Nil):
  def add[A: Value](const: IR.Constant[A]): (RefIR[A], ConstantsManager) =
    ???
  def output: List[IR[?]] = block.reverse
