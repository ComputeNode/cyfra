package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR

case class DebugManager(block: List[IR[?]] = Nil):
  def add(ir: IR[?]): DebugManager = ???
  def output: List[IR[?]] = block.reverse
