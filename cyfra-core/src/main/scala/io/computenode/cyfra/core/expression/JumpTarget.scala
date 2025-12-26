package io.computenode.cyfra.core.expression

import io.computenode.cyfra.utility.Utility.nextId

class JumpTarget[A: Value]:
  val id: Int = nextId()
  override def toString: String = s"jt#$id"

  override def hashCode(): Int = id + 1
  override def equals(obj: Any): Boolean = obj match
    case value: JumpTarget[A] => value.id == id
    case _                    => false
