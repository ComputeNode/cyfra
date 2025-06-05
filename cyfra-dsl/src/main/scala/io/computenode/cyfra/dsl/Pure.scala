package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.dsl.Control.Scope
import io.computenode.cyfra.dsl.Expression.FunctionCall
import io.computenode.cyfra.dsl.macros.FnCall
import izumi.reflect.Tag

object Pure:
  def pure[V <: Value : FromExpr: Tag](f: => V)(using fnCall: FnCall): V =
    val call = FunctionCall[V](fnCall.identifier, Scope(f.tree.asInstanceOf[Expression[V]], isDetached = true), fnCall.params)
    summon[FromExpr[V]].fromExpr(call)
