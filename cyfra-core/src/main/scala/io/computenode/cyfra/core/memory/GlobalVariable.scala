package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.memory.GlobalVariable.Sharing
import io.computenode.cyfra.core.memory.GlobalVariable.Sharing.Private

class GlobalVariable[T: Value](init: Option[T] = None, val sharing: Sharing = Private) extends Variable[T]:
  override def toString: String = s"$sharing var#$id"

object GlobalVariable:
  enum Sharing:
    case Private, Workgroup, CrossWorkgroup
