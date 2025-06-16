package io.computenode.cyfra.dsl.struct

import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.macros.Source

trait GStructConstructor[T <: GStruct[T]] extends FromExpr[T]:
  def schema: GStructSchema[T]
  def fromExpr(expr: E[T])(using Source): T
