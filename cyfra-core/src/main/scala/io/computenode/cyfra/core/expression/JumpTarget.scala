package io.computenode.cyfra.core.expression

import io.computenode.cyfra.utility.Utility.nextId

class JumpTarget[A: Value]:
  val id: Int = nextId()
  override def toString: String = s"jt#$id"
