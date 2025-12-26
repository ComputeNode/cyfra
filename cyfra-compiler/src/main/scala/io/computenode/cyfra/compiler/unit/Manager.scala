package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR

trait Manager:
  def output: List[IR[?]]
