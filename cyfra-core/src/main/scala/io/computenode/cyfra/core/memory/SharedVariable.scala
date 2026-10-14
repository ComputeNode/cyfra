package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value

/** A variable shared by invocations in one workgroup. */
final class SharedVariable[T: Value] extends Variable[T]
