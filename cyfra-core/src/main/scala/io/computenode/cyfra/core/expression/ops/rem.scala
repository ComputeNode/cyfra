package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

// Matrix * Scalar
extension [T <: FloatType: Value, M <: Mat[T]: Value](mat: M)
  @targetName("matrixTimesScalar")
  def *(scalar: T): M = Value.map(Operator.MatrixTimesScalar)(mat, scalar)

def select[T: Value](cond: Bool, obj1: T, obj2: T): T = Value.map(Operator.Select)(cond, obj1, obj2)

def select[V <: Vec[Bool]: Value, T <: Vec[?]: Value](cond: V, obj1: T, obj2: T): T = Value.map(Operator.Select)(cond, obj1, obj2)
