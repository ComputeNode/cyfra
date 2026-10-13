package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.types.{FloatType, Scalar}
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

trait MatOps[T <: Scalar: Value, Mat[_ <: Scalar]](using Value[Mat[T]]):
  this: Mat[T] =>
  private val self: Mat[T] = this

  @targetName("matrixTimesScalar")
  def *(scalar: T)(using T <:< FloatType): Mat[T] = Value.map(Operator.MatrixTimesScalar)(self, scalar)
