package io.computenode.cyfra.compiler.ir

import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.Var

case class FunctionIR[A: Value](name: String, parameters: List[Var[?]]):
  def v: Value[A] = summon[Value[A]]
