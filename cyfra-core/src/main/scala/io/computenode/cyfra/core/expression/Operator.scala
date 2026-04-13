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

  // GLSL extended section

  // universal
  case object Abs extends Operator1
  case object Sign extends Operator1
  case object Min extends Operator2
  case object Max extends Operator2
  case object Clamp extends Operator3

  // integer only
  case object FindLsb extends Operator1
  case object FindMsb extends Operator1

  // float only
  case object Round extends Operator1
  case object RoundEven extends Operator1
  case object Trunc extends Operator1
  case object Floor extends Operator1
  case object Ceil extends Operator1
  case object Fract extends Operator1
  case object Radians extends Operator1
  case object Degrees extends Operator1
  case object Sin extends Operator1
  case object Cos extends Operator1
  case object Tan extends Operator1
  case object Asin extends Operator1
  case object Acos extends Operator1
  case object Atan extends Operator1
  case object Sinh extends Operator1
  case object Cosh extends Operator1
  case object Tanh extends Operator1
  case object Asinh extends Operator1
  case object Acosh extends Operator1
  case object Atanh extends Operator1
  case object Exp extends Operator1
  case object Log extends Operator1
  case object Exp2 extends Operator1
  case object Log2 extends Operator1
  case object Sqrt extends Operator1
  case object InverseSqrt extends Operator1
  case object ModfStruct extends Operator1
  case object FrexpStruct extends Operator1
  case object Length extends Operator1
  case object Normalize extends Operator1
  case object InterpolateAtCentroid extends Operator1
  case object Atan2 extends Operator2
  case object Pow extends Operator2
  case object Modf extends Operator2
  case object Frexp extends Operator2
  case object Ldexp extends Operator2
  case object Step extends Operator2
  case object Distance extends Operator2
  case object Reflect extends Operator2
  case object NMin extends Operator2
  case object NMax extends Operator2
  case object InterpolateAtSample extends Operator2
  case object InterpolateAtOffset extends Operator2
  case object FMix extends Operator3
  case object SmoothStep extends Operator3
  case object Fma extends Operator3
  case object FaceForward extends Operator3
  case object Refract extends Operator3
  case object NClamp extends Operator3

  // vector and matrix
  case object Cross extends Operator2
  case object Determinant extends Operator1
  case object MatrixInverse extends Operator1

  // pack/unpack
  case object PackSnorm4x8 extends Operator1
  case object PackUnorm4x8 extends Operator1
  case object PackSnorm2x16 extends Operator1
  case object PackUnorm2x16 extends Operator1
  case object PackHalf2x16 extends Operator1
  case object PackDouble2x32 extends Operator1
  case object UnpackSnorm2x16 extends Operator1
  case object UnpackUnorm2x16 extends Operator1
  case object UnpackHalf2x16 extends Operator1
  case object UnpackSnorm4x8 extends Operator1
  case object UnpackUnorm4x8 extends Operator1
  case object UnpackDouble2x32 extends Operator1
