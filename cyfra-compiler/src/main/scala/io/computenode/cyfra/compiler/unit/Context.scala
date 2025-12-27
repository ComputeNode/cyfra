package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR

case class Context(
  prefix: List[IR[?]],
  private[unit] debug: DebugManager,
  private[unit] types: TypeManager,
  private[unit] constants: ConstantsManager,
):
  def output: List[IR[?]] = prefix ++ debug.output ++ types.output ++ constants.output
