package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.types.GArray

/** A fixed-size array shared by invocations in one workgroup. */
final class GShared[T: Value](val length: Int) extends Variable[GArray[T]]:
  require(length > 0, "Shared array length must be positive")
  def elementValue: Value[T] = Value[T]
