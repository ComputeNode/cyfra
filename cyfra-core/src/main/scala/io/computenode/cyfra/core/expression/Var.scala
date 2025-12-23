package io.computenode.cyfra.core.expression

import io.computenode.cyfra.utility.Utility.nextId

class Var[T: Value]:
  val id: Int = nextId()
  override def toString: String = s"var#$id"

