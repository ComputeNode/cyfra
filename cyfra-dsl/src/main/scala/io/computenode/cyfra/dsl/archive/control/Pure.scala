package io.computenode.cyfra.dsl.archive.control

import io.computenode.cyfra.dsl.archive.Expression.FunctionCall
import io.computenode.cyfra.dsl.archive.Value.FromExpr
import io.computenode.cyfra.dsl.archive.macros.FnCall
import io.computenode.cyfra.dsl.archive.{Expression, Value}
import izumi.reflect.Tag

object Pure:
  def pure[V <: Value: {FromExpr, Tag}](f: => V)(using fnCall: FnCall): V =
    val call = FunctionCall[V](fnCall.identifier, Scope(f.tree.asInstanceOf[Expression[V]], isDetached = true), fnCall.params)
    summon[FromExpr[V]].fromExpr(call)
