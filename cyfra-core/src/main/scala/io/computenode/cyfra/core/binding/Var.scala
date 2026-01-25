package io.computenode.cyfra.core.binding

import io.computenode.cyfra.core.binding.FocusRoot
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.utility.Utility.nextId

class Var[T: Value] extends FocusRoot[T]:
  val id: Int = nextId()
  override def toString: String = s"var#$id"
  

