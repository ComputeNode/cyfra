package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR

case class Context(prefix: List[IR[?]], decorations: List[IR[?]], types: TypeManager, constants: ConstantsManager, suffix: List[IR[?]]):
  def output: List[IR[?]] = prefix ++ decorations ++ types.output ++ constants.output ++ suffix
