package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

abstract class Operator:
  def name: String = this.getClass.getSimpleName.replace("$", "")
  override def toString: String = s"operator $name"

object Operator:
  abstract class Operator0 extends Operator
  abstract class Operator1 extends Operator
  abstract class Operator2 extends Operator
  abstract class Operator3 extends Operator
  abstract class Operator4 extends Operator

  // Concreate type operations
  case object Add extends Operator2
  case object Sub extends Operator2
  case object Mul extends Operator2
  case object Div extends Operator2
  case object Mod extends Operator2

  // Negative type operations
  case object Neg extends Operator1
  case object Rem extends Operator2

  // Vector/Matrix operations
  case object VectorTimesScalar extends Operator2
  case object MatrixTimesScalar extends Operator2
  case object VectorTimesMatrix extends Operator2
  case object MatrixTimesVector extends Operator2
  case object MatrixTimesMatrix extends Operator2
  case object OuterProduct extends Operator2
  case object Dot extends Operator2

  // Bitwise operations
  case object ShiftRightLogical extends Operator2
  case object ShiftRightArithmetic extends Operator2
  case object ShiftLeftLogical extends Operator2
  case object BitwiseOr extends Operator2
  case object BitwiseXor extends Operator2
  case object BitwiseAnd extends Operator2
  case object BitwiseNot extends Operator1
  case object BitFieldInsert extends Operator4
  case object BitFieldExtract extends Operator3
  case object BitReverse extends Operator1
  case object BitCount extends Operator1

  // Logical operations on booleans
  case object LogicalAny extends Operator1
  case object LogicalAll extends Operator1
  case object LogicalEqual extends Operator2
  case object LogicalNotEqual extends Operator2
  case object LogicalOr extends Operator2
  case object LogicalAnd extends Operator2
  case object LogicalNot extends Operator1

  // Floating-point checks
  case object IsNan extends Operator1
  case object IsInf extends Operator1
  case object IsFinite extends Operator1
  case object IsNormal extends Operator1
  case object SignBitSet extends Operator1

  // Comparisons
  case object Equal extends Operator2
  case object NotEqual extends Operator2
  case object LessThan extends Operator2
  case object GreaterThan extends Operator2
  case object LessThanEqual extends Operator2
  case object GreaterThanEqual extends Operator2

  // Select
  case object Select extends Operator3

  // Composite
  case object CompositeExtract extends Operator2
  case object CompositeInsert extends Operator3

  case object VectorShuffle extends Operator3
