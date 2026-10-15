package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR

case class Context(extensions: ExtensionManager, prefix: List[IR[?]], decorations: List[IR[?]], types: TypeManager, suffix: List[IR[?]]):
  def output: List[IR[?]] = extensions.output ++ prefix ++ decorations ++ types.output ++ suffix
