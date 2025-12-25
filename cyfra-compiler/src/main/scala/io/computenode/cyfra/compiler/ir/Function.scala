package io.computenode.cyfra.compiler.ir

import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.Var

case class Function[A: Value](name: String, parameters: List[Var[?]], body: IRs[A])
