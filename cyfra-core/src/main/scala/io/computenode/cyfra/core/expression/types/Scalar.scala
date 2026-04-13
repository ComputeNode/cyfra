package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*

private[types] def const[A: Value](value: Any): A =
  Value[A].extract(ExpressionBlock(Expression.Constant[A](value)))

sealed trait Scalar

trait BoolType extends Scalar

sealed trait NumericalType extends Scalar
sealed trait NegativeType extends NumericalType

trait FloatType extends NegativeType

sealed trait IntegerType extends NumericalType
trait SignedIntType extends IntegerType with NegativeType
trait UnsignedIntType extends IntegerType
