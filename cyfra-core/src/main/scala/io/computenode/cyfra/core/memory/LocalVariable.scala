package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value

class LocalVariable[T: Value] private[cyfra](shared: Boolean = false)  extends Variable[T]
