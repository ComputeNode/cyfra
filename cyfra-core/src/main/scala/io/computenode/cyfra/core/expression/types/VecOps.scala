package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.{Expression, Value}

trait VecOps[T: Value]

object VecOps:
  private[types] def extract[T: Value, CC: Value](cc: CC, i: Int): T =
    val s = Value[CC].peel(cc)
    Value[T].extract(s.add(Expression.Extract(s.result, i)))

  private[types] def insert[T: Value, CC: Value](cc: CC, v: T, i: Int): CC =
    val s = Value[CC].peel(cc)
    val sv = Value[T].peel(v)
    Value[CC].extract(s.extend(sv).add(Expression.Insert(s.result, sv.result, i)))
