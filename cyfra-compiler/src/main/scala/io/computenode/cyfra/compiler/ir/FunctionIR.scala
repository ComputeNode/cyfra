package io.computenode.cyfra.compiler.ir

import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.core.binding.Variable
import io.computenode.cyfra.core.expression.Value

case class FunctionIR[A: Value](name: String, parameters: List[Variable[?]]):
  def v: Value[A] = summon[Value[A]]
