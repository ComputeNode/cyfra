package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*

trait Vec[T <: Scalar: Value] extends VecOps[T]
