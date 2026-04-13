package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.*

def select[T: Value](cond: Bool, obj1: T, obj2: T): T = Value.map(Operator.Select)(cond, obj1, obj2)

def select[V <: Vec[Bool]: Value, T <: Vec[?]: Value](cond: V, obj1: T, obj2: T): T = Value.map(Operator.Select)(cond, obj1, obj2)
