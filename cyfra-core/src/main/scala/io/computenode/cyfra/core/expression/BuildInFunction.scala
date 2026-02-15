package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

abstract class BuildInFunction(val isPure: Boolean):
  def name: String = this.getClass.getSimpleName.replace("$", "")
  override def toString: String = s"builtin $name"

object BuildInFunction:
  abstract class BuildInFunction0(isPure: Boolean) extends BuildInFunction(isPure)
  abstract class BuildInFunction1(isPure: Boolean) extends BuildInFunction(isPure)
  abstract class BuildInFunction2(isPure: Boolean) extends BuildInFunction(isPure)
  abstract class BuildInFunction3(isPure: Boolean) extends BuildInFunction(isPure)
  abstract class BuildInFunction4(isPure: Boolean) extends BuildInFunction(isPure)

  // Concreate type operations
  case object Add extends BuildInFunction2(true)
  case object Sub extends BuildInFunction2(true)
  case object Mul extends BuildInFunction2(true)
  case object Div extends BuildInFunction2(true)
  case object Mod extends BuildInFunction2(true)

  // Negative type operations
  case object Neg extends BuildInFunction1(true)
  case object Rem extends BuildInFunction2(true)

  // Vector/Matrix operations
  case object VectorTimesScalar extends BuildInFunction2(true)
  case object MatrixTimesScalar extends BuildInFunction2(true)
  case object VectorTimesMatrix extends BuildInFunction2(true)
  case object MatrixTimesVector extends BuildInFunction2(true)
  case object MatrixTimesMatrix extends BuildInFunction2(true)
  case object OuterProduct extends BuildInFunction2(true)
  case object Dot extends BuildInFunction2(true)

  // Bitwise operations
  case object ShiftRightLogical extends BuildInFunction2(true)
  case object ShiftRightArithmetic extends BuildInFunction2(true)
  case object ShiftLeftLogical extends BuildInFunction2(true)
  case object BitwiseOr extends BuildInFunction2(true)
  case object BitwiseXor extends BuildInFunction2(true)
  case object BitwiseAnd extends BuildInFunction2(true)
  case object BitwiseNot extends BuildInFunction1(true)
  case object BitFieldInsert extends BuildInFunction4(true)
  case object BitFieldExtract extends BuildInFunction3(true)
  case object BitReverse extends BuildInFunction1(true)
  case object BitCount extends BuildInFunction1(true)

  // Logical operations on booleans
  case object LogicalAny extends BuildInFunction1(true)
  case object LogicalAll extends BuildInFunction1(true)
  case object LogicalEqual extends BuildInFunction2(true)
  case object LogicalNotEqual extends BuildInFunction2(true)
  case object LogicalOr extends BuildInFunction2(true)
  case object LogicalAnd extends BuildInFunction2(true)
  case object LogicalNot extends BuildInFunction1(true)

  // Floating-point checks
  case object IsNan extends BuildInFunction1(true)
  case object IsInf extends BuildInFunction1(true)
  case object IsFinite extends BuildInFunction1(true)
  case object IsNormal extends BuildInFunction1(true)
  case object SignBitSet extends BuildInFunction1(true)

  // Comparisons
  case object Equal extends BuildInFunction2(true)
  case object NotEqual extends BuildInFunction2(true)
  case object LessThan extends BuildInFunction2(true)
  case object GreaterThan extends BuildInFunction2(true)
  case object LessThanEqual extends BuildInFunction2(true)
  case object GreaterThanEqual extends BuildInFunction2(true)

  // Select
  case object Select extends BuildInFunction3(true)
