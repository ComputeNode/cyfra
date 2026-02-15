package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value

class GlobalVariable[T: Value](init: Option[T] = None, val sharing: "private" | "workgroup" | "cross-workgroup" = "private") extends Variable[T]:
  override def toString: String = s"$sharing var#$id"
