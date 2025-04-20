package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.Value.GBoolean
import izumi.reflect.Tag

import java.util.UUID

object Control:
  
  case class Scope[T <: Value : Tag](expr: Expression[T]):
    def rootTreeId: Int = expr.treeid
  
  case class When[T <: Value: Tag : FromExpr](
    when: GBoolean, 
    thenCode: T,
    otherConds: List[Scope[GBoolean]],
    otherCases: List[Scope[T]],
    name: sourcecode.Name
  ):
    def elseWhen(cond: GBoolean)(t: T): When[T] =
      When(when, thenCode, otherConds :+ Scope(cond.tree), otherCases :+ Scope(t.tree.asInstanceOf[E[T]]), name)
    def otherwise(t: T): T =
      summon[FromExpr[T]].fromExpr(WhenExpr(when, Scope(thenCode.tree.asInstanceOf[E[T]]), otherConds, otherCases, Scope(t.tree.asInstanceOf[E[T]])))(using name)
 
  case class WhenExpr[T <: Value: Tag](
    when: GBoolean, 
    thenCode: Scope[T], 
    otherConds: List[Scope[GBoolean]], 
    otherCaseCodes: List[Scope[T]],
    otherwise: Scope[T]
  ) extends Expression[T]
  
  def when[T <: Value: Tag: FromExpr](cond: GBoolean)(fn: T)(using name: sourcecode.Name): When[T] =
    When(cond, fn, Nil, Nil, name)
    
