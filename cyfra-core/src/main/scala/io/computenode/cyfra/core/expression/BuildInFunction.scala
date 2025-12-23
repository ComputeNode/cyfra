package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.expression.*

abstract class BuildInFunction[-R](val isPure: Boolean):
  override def toString: String = s"builtin ${this.getClass.getSimpleName.replace("$", "")}"

object BuildInFunction:
  abstract class BuildInFunction0[-R](isPure: Boolean) extends BuildInFunction[R](isPure)
  abstract class BuildInFunction1[-A1, -R](isPure: Boolean) extends BuildInFunction[R](isPure)
  abstract class BuildInFunction1R[-R](isPure: Boolean) extends BuildInFunction1[R, R](isPure)
  abstract class BuildInFunction2[-A1, -A2, -R](isPure: Boolean) extends BuildInFunction[R](isPure)
  abstract class BuildInFunction2R[-R](isPure: Boolean) extends BuildInFunction2[R, R, R](isPure)
  abstract class BuildInFunction3[-A1, -A2, -A3, -R](isPure: Boolean) extends BuildInFunction[R](isPure)
  abstract class BuildInFunction4[-A1, -A2, -A3, -A4, -R](isPure: Boolean) extends BuildInFunction[R](isPure)

  // Concreate type operations
  case object Add extends BuildInFunction2R[Any](true)
  case object Sub extends BuildInFunction2R[Any](true)
  case object Mul extends BuildInFunction2R[Any](true)
  case object Div extends BuildInFunction2R[Any](true)
  case object Mod extends BuildInFunction2R[Any](true)

  // Negative type operations
  case object Neg extends BuildInFunction1R[Any](true)
  case object Rem extends BuildInFunction2R[Any](true)
  
  // Vector/Matrix operations
  case object VectorTimesScalar extends BuildInFunction2[Any, Any, Any](true)
  case object MatrixTimesScalar extends BuildInFunction2[Any, Any, Any](true)
  case object VectorTimesMatrix extends BuildInFunction2[Any, Any, Any](true)
  case object MatrixTimesVector extends BuildInFunction2[Any, Any, Any](true)
  case object MatrixTimesMatrix extends BuildInFunction2R[Any](true)
  case object OuterProduct extends BuildInFunction2[Any, Any, Any](true)
  case object Dot extends BuildInFunction2[Any, Any, Any](true)

  // Bitwise operations
  case object ShiftRightLogical extends BuildInFunction2R[Any](true)
  case object ShiftRightArithmetic extends BuildInFunction2R[Any](true)
  case object ShiftLeftLogical extends BuildInFunction2R[Any](true)
  case object BitwiseOr extends BuildInFunction2R[Any](true)
  case object BitwiseXor extends BuildInFunction2R[Any](true)
  case object BitwiseAnd extends BuildInFunction2R[Any](true)
  case object BitwiseNot extends BuildInFunction1R[Any](true)
  case object BitFieldInsert extends BuildInFunction4[Any, Any, Any, Any, Any](true)
  case object BitFieldSExtract extends BuildInFunction3[Any, Any, Any, Any](true)
  case object BitFieldUExtract extends BuildInFunction3[Any, Any, Any, Any](true)
  case object BitReverse extends BuildInFunction1R[Any](true)
  case object BitCount extends BuildInFunction1[Any, Any](true)

  // Logical operations on booleans
  case object LogicalAny extends BuildInFunction1[Any, Bool](true)
  case object LogicalAll extends BuildInFunction1[Any, Bool](true)
  case object LogicalEqual extends BuildInFunction2R[Any](true)
  case object LogicalNotEqual extends BuildInFunction2R[Any](true)
  case object LogicalOr extends BuildInFunction2R[Any](true)
  case object LogicalAnd extends BuildInFunction2R[Any](true)
  case object LogicalNot extends BuildInFunction1R[Any](true)

  // Floating-point checks
  case object IsNan extends BuildInFunction1[Any, Any](true)
  case object IsInf extends BuildInFunction1[Any, Any](true)
  case object IsFinite extends BuildInFunction1[Any, Any](true)
  case object IsNormal extends BuildInFunction1[Any, Any](true)
  case object SignBitSet extends BuildInFunction1[Any, Any](true)

  // Comparisons 
  case object Equal extends BuildInFunction2[Any, Any, Any](true)
  case object NotEqual extends BuildInFunction2[Any, Any, Any](true)
  case object LessThan extends BuildInFunction2[Any, Any, Any](true)
  case object GreaterThan extends BuildInFunction2[Any, Any, Any](true)
  case object LessThanEqual extends BuildInFunction2[Any, Any, Any](true)
  case object GreaterThanEqual extends BuildInFunction2[Any, Any, Any](true)

  // Select
  case object Select extends BuildInFunction3[Any, Any, Any, Any](true)
