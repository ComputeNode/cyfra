package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR

case class Context(
  prefix: List[IR[?]],
  debug: DebugManager,
  decorations: List[IR[?]],
  types: TypeManager,
  constants: ConstantsManager,
  suffix: List[IR[?]],
):
  def output: List[IR[?]] = prefix ++ debug.output ++ decorations ++ types.output ++ constants.output ++ suffix
