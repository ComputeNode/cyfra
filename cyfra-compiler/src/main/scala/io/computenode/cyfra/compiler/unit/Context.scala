package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR

case class Context(prefix: List[IR[?]], debug: DebugManager, types: TypeManager, constants: ConstantsManager):
  def output: List[IR[?]] = prefix ++ debug.output ++ types.output ++ constants.output
