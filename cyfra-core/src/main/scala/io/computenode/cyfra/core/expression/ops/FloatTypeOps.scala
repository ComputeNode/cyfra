package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.{Bool, FloatType, Mat, Vec}
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

trait FloatTypeOps[T <: FloatType: Value] extends NegativeTypeOps[T]:
  this: T =>
  private val self: T = this

  @targetName("scalarTimesMatrix")
  def m_*[M <: Mat](mat: M[T])(using Value[M[T]]): M[T] = Value.map(Operator.MatrixTimesScalar)(mat, self)

  @targetName("scalarTimesVector")
  def *[V <: Vec](vec: V[T])(using Value[V[T]]): V[T] = Value.map(Operator.VectorTimesScalar)(vec, self)

  def isNan: Bool = Value.map(Operator.IsNan)(self)

  def isInf: Bool = Value.map(Operator.IsInf)(self)

  def isFinite: Bool = Value.map(Operator.IsFinite)(self)

  def isNormal: Bool = Value.map(Operator.IsNormal)(self)

  def signBitSet: Bool = Value.map(Operator.SignBitSet)(self)
