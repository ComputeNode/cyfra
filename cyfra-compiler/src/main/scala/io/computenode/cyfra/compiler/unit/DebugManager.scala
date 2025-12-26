package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR

class DebugManager extends Manager:
  private val block: List[IR[?]] = Nil


  def output: List[IR[?]] = block.reverse
