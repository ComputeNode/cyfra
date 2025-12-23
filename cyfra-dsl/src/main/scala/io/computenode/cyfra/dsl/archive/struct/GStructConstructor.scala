package io.computenode.cyfra.dsl.archive.struct

import io.computenode.cyfra.dsl.archive.Expression.E
import io.computenode.cyfra.dsl.archive.Value.FromExpr
import io.computenode.cyfra.dsl.archive.macros.Source

trait GStructConstructor[T <: GStruct[T]] extends FromExpr[T]:
  def schema: GStructSchema[T]
  def fromExpr(expr: E[T])(using Source): T
