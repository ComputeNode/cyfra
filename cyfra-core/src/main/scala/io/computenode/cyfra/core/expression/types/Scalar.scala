package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.BoolOps

sealed trait Scalar

sealed trait NumericalType extends Scalar
sealed trait NegativeType extends NumericalType
sealed trait IntegerType extends NumericalType

trait BoolType extends Scalar
trait FloatType extends NegativeType
trait SignedIntType extends IntegerType with NegativeType
trait UnsignedIntType extends IntegerType
