package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.{Expression, Operator, Value}

import scala.annotation.targetName

trait VecOps[T <: Scalar: Value, Vec[_ <: Scalar]](using Value[Vec[T]], Value[Vec[Bool]])
    extends VecBoolOps[T, Vec]
    with VecFloatOps[T, Vec]
    with VecIntegerOps[T, Vec]
    with VecNegativeOps[T, Vec]
    with VecNumericalOps[T, Vec]:
  this: Vec[T] =>
  private val self: Vec[T] = this

  @targetName("equal")
  def ===(that: Vec[T]): Vec[Bool] = Value.map(Operator.Equal)(self, that)

  @targetName("notEqual")
  def !==(that: Vec[T]): Vec[Bool] = Value.map(Operator.NotEqual)(self, that)

object VecOps:
  private[ops] def extract[T: Value, CC: Value](cc: CC, i: Int): T =
    val s = Value[CC].peel(cc)
    Value[T].extract(s.add(Expression.Extract(s.result, i)))

  private[ops] def insert[T: Value, CC: Value](cc: CC, v: T, i: Int): CC =
    val s = Value[CC].peel(cc)
    val sv = Value[T].peel(v)
    Value[CC].extract(s.extend(sv).add(Expression.Insert(s.result, sv.result, i)))
