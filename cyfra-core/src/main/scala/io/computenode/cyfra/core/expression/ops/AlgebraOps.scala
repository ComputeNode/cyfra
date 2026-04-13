package io.computenode.cyfra.core.expression.ops

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Value.map
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.{Operator, Value}

import scala.annotation.targetName

given [T <: NumericalType: Value]: NumericalOps[T] with {}

trait NumericalOps[T]

extension [T: {NumericalOps, Value}](self: T)
  @targetName("add")
  def +(that: T): T = Value.map(Operator.Add)(self, that)
  @targetName("sub")
  def -(that: T): T = Value.map(Operator.Sub)(self, that)
  @targetName("mul")
  def *(that: T): T = Value.map(Operator.Mul)(self, that)
  @targetName("div")
  def /(that: T): T = Value.map(Operator.Div)(self, that)
  @targetName("mod")
  def %(that: T): T = Value.map(Operator.Mod)(self, that)

extension [T <: FloatType: Value, V <: Vec[T]: Value](scalar: T)
  @targetName("scalarTimesVector")
  def *(vec: V): V = Value.map(Operator.VectorTimesScalar)(vec, scalar)

// Matrix * Scalar
extension [T <: FloatType: Value, M <: Mat[T]: Value](mat: M)
  @targetName("matrixTimesScalar")
  def *(scalar: T): M = Value.map(Operator.MatrixTimesScalar)(mat, scalar)

extension [T <: FloatType: Value, M <: Mat[T]: Value](scalar: T)
  @targetName("scalarTimesMatrix")
  def *(mat: M): M = Value.map(Operator.MatrixTimesScalar)(mat, scalar)

// Dot product: Vec * Vec -> Scalar
extension [T <: FloatType: Value, V <: Vec[T]: Value](v1: V)
  @targetName("dotProduct")
  infix def dot(v2: V): T = Value.map(Operator.Dot)[V, V, T](v1, v2)
