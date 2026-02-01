package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.memory.FocusRoot
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.utility.Utility.nextId

trait Variable[T: Value] extends FocusRoot[T]:
  val id: Int = nextId()
  override def toString: String = s"var#$id"
  

