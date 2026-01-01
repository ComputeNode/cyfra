package io.computenode.cyfra.core.expression

import io.computenode.cyfra.utility.Utility.nextId

class Var[T: Value]:
  def v: Value[T] = Value[T]
  val id: Int = nextId()
  override def toString: String = s"var#$id"

