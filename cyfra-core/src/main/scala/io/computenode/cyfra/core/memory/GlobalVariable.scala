package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value

class GlobalVariable[T: Value](init: Option[T] = None, shared: Boolean = false) extends Variable[T](shared)
