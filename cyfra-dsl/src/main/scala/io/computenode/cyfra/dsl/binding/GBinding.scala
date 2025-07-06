package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import izumi.reflect.Tag

trait GBinding[T <: Value: Tag: FromExpr]
