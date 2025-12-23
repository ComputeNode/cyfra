package io.computenode.cyfra.dsl.archive.control

import When.WhenExpr
import io.computenode.cyfra.dsl.archive.Expression.E
import io.computenode.cyfra.dsl.archive.{Expression, Value}
import io.computenode.cyfra.dsl.archive.Value.{FromExpr, GBoolean}
import io.computenode.cyfra.dsl.archive.macros.Source
import izumi.reflect.Tag

case class When[T <: Value: {Tag, FromExpr}](
  when: GBoolean,
  thenCode: T,
  otherConds: List[Scope[GBoolean]],
  otherCases: List[Scope[T]],
  name: Source,
):
  def elseWhen(cond: GBoolean)(t: T): When[T] =
    When(when, thenCode, otherConds :+ Scope(cond.tree), otherCases :+ Scope(t.tree.asInstanceOf[E[T]]), name)
  infix def otherwise(t: T): T =
    summon[FromExpr[T]]
      .fromExpr(WhenExpr(when, Scope(thenCode.tree.asInstanceOf[E[T]]), otherConds, otherCases, Scope(t.tree.asInstanceOf[E[T]])))(using name)

object When:

  case class WhenExpr[T <: Value: Tag](
    when: GBoolean,
    thenCode: Scope[T],
    otherConds: List[Scope[GBoolean]],
    otherCaseCodes: List[Scope[T]],
    otherwise: Scope[T],
  ) extends Expression[T]

  def when[T <: Value: {Tag, FromExpr}](cond: GBoolean)(fn: T)(using name: Source): When[T] =
    When(cond, fn, Nil, Nil, name)
