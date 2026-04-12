package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.{Context, Ctx}
import io.computenode.cyfra.compiler.Spirv.{Code, GlslOp, Op}
import io.computenode.cyfra.compiler.modules.Algebra.GlslStd450
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Operator.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.Tag

class Algebra extends FunctionCompilationModule:
  def compileFunction(input: IRs[?])(using Ctx): IRs[?] =
    input.flatMapReplace:
      case x: IR.Operation[a] => handleOperation[a](x)(using x.v)
      case other              => IRs(other)(using other.v)

  private def handleOperation[A: Value](operation: IR.Operation[A])(using Ctx): IRs[A] =
    val IR.Operation(operator, args) = operation
    val argBaseTag = args.head.v.bottomComposite.tag
    val tpe = Ctx.getType(Value[A])

    val nativeFinder: PartialFunction[Operator, Code] = argBaseTag match
      case t if t <:< Tag[FloatType]       => findFloat
      case t if t <:< Tag[SignedIntType]   => findInteger orElse findSignedInteger
      case t if t <:< Tag[UnsignedIntType] => findInteger orElse findUnsignedInteger
      case t if t =:= Tag[Bool]            => findBoolean
      case _                               => PartialFunction.empty

    val glslFinder: PartialFunction[Operator, Code] = argBaseTag match
      case t if t <:< Tag[FloatType]       => findGlslFloat
      case t if t <:< Tag[SignedIntType]   => findGlslSigned
      case t if t <:< Tag[UnsignedIntType] => findGlslUnsigned
      case _                               => PartialFunction.empty

    if findComposite.isDefinedAt(operator) then return IRs(IR.SvRef[A](findComposite(operator), tpe, args))

    nativeFinder.lift(operator) match
      case Some(opCode) => return IRs(IR.SvRef[A](opCode, tpe, args))
      case None         => ()

    glslFinder.lift(operator) match
      case Some(code) =>
        val importRef = Ctx.getExtInstImport(GlslStd450)
        return IRs(IR.SvRef[A](Op.OpExtInst, tpe, importRef :: code :: args))
      case None => ()
    throw CompilationException(s"Operator $operator not supported for $argBaseTag")

  private val findComposite: PartialFunction[Operator, Code] =
    case CompositeExtract => Op.OpCompositeExtract
    case CompositeInsert  => Op.OpCompositeInsert
    case VectorShuffle    => Op.OpVectorShuffle

  // Native SPIR-V instructions
  private val findFloat: PartialFunction[Operator, Code] =
    case Add => Op.OpFAdd
    case Sub => Op.OpFSub
    case Mul => Op.OpFMul
    case Div => Op.OpFDiv
    case Mod => Op.OpFMod

    case Neg => Op.OpFNegate
    case Rem => Op.OpFRem

    case IsNan      => Op.OpIsNan
    case IsInf      => Op.OpIsInf
    case IsFinite   => Op.OpIsFinite
    case IsNormal   => Op.OpIsNormal
    case SignBitSet => Op.OpSignBitSet

    case VectorTimesScalar => Op.OpVectorTimesScalar
    case MatrixTimesScalar => Op.OpMatrixTimesScalar
    case VectorTimesMatrix => Op.OpVectorTimesMatrix
    case MatrixTimesVector => Op.OpMatrixTimesVector
    case MatrixTimesMatrix => Op.OpMatrixTimesMatrix
    case OuterProduct      => Op.OpOuterProduct
    case Dot               => Op.OpDot

    case Equal            => Op.OpFOrdEqual
    case NotEqual         => Op.OpFOrdNotEqual
    case LessThan         => Op.OpFOrdLessThan
    case GreaterThan      => Op.OpFOrdGreaterThan
    case LessThanEqual    => Op.OpFOrdLessThanEqual
    case GreaterThanEqual => Op.OpFOrdGreaterThanEqual

  private val findBoolean: PartialFunction[Operator, Code] =
    case LogicalAny      => Op.OpAny
    case LogicalAll      => Op.OpAll
    case LogicalEqual    => Op.OpLogicalEqual
    case LogicalNotEqual => Op.OpLogicalNotEqual
    case LogicalOr       => Op.OpLogicalOr
    case LogicalAnd      => Op.OpLogicalAnd
    case LogicalNot      => Op.OpLogicalNot
    case Select          => Op.OpSelect // This code need more research

  private val findInteger: PartialFunction[Operator, Code] =
    case Add => Op.OpIAdd
    case Sub => Op.OpISub
    case Mul => Op.OpIMul

    case ShiftRightLogical    => Op.OpShiftRightLogical
    case ShiftRightArithmetic => Op.OpShiftRightArithmetic
    case ShiftLeftLogical     => Op.OpShiftLeftLogical
    case BitwiseOr            => Op.OpBitwiseOr
    case BitwiseXor           => Op.OpBitwiseXor
    case BitwiseAnd           => Op.OpBitwiseAnd
    case BitwiseNot           => Op.OpNot
    case BitFieldInsert       => Op.OpBitFieldInsert
    case BitReverse           => Op.OpBitReverse
    case BitCount             => Op.OpBitCount

    case Equal    => Op.OpIEqual
    case NotEqual => Op.OpINotEqual

  private val findSignedInteger: PartialFunction[Operator, Code] =
    case Div => Op.OpSDiv
    case Mod => Op.OpSMod

    case Neg => Op.OpSNegate
    case Rem => Op.OpSRem

    case BitFieldExtract => Op.OpBitFieldSExtract

    case LessThan         => Op.OpSLessThan
    case GreaterThan      => Op.OpSGreaterThan
    case LessThanEqual    => Op.OpSLessThanEqual
    case GreaterThanEqual => Op.OpSGreaterThanEqual

  private val findUnsignedInteger: PartialFunction[Operator, Code] =
    case Div => Op.OpUDiv
    case Mod => Op.OpUMod

    case BitFieldExtract => Op.OpBitFieldUExtract

    case LessThan         => Op.OpULessThan
    case GreaterThan      => Op.OpUGreaterThan
    case LessThanEqual    => Op.OpULessThanEqual
    case GreaterThanEqual => Op.OpUGreaterThanEqual

  // GLSL extended instructions
  private val findGlslFloat: PartialFunction[Operator, Code] =
    case Abs                   => GlslOp.FAbs
    case Sign                  => GlslOp.FSign
    case Min                   => GlslOp.FMin
    case Max                   => GlslOp.FMax
    case Clamp                 => GlslOp.FClamp
    case Round                 => GlslOp.Round
    case RoundEven             => GlslOp.RoundEven
    case Trunc                 => GlslOp.Trunc
    case Floor                 => GlslOp.Floor
    case Ceil                  => GlslOp.Ceil
    case Fract                 => GlslOp.Fract
    case Radians               => GlslOp.Radians
    case Degrees               => GlslOp.Degrees
    case Sin                   => GlslOp.Sin
    case Cos                   => GlslOp.Cos
    case Tan                   => GlslOp.Tan
    case Asin                  => GlslOp.Asin
    case Acos                  => GlslOp.Acos
    case Atan                  => GlslOp.Atan
    case Sinh                  => GlslOp.Sinh
    case Cosh                  => GlslOp.Cosh
    case Tanh                  => GlslOp.Tanh
    case Asinh                 => GlslOp.Asinh
    case Acosh                 => GlslOp.Acosh
    case Atanh                 => GlslOp.Atanh
    case Exp                   => GlslOp.Exp
    case Log                   => GlslOp.Log
    case Exp2                  => GlslOp.Exp2
    case Log2                  => GlslOp.Log2
    case Sqrt                  => GlslOp.Sqrt
    case InverseSqrt           => GlslOp.InverseSqrt
    case ModfStruct            => GlslOp.ModfStruct
    case FrexpStruct           => GlslOp.FrexpStruct
    case Length                => GlslOp.Length
    case Normalize             => GlslOp.Normalize
    case InterpolateAtCentroid => GlslOp.InterpolateAtCentroid
    case Atan2                 => GlslOp.Atan2
    case Pow                   => GlslOp.Pow
    case Modf                  => GlslOp.Modf
    case Frexp                 => GlslOp.Frexp
    case Ldexp                 => GlslOp.Ldexp
    case Step                  => GlslOp.Step
    case Distance              => GlslOp.Distance
    case Reflect               => GlslOp.Reflect
    case NMin                  => GlslOp.NMin
    case NMax                  => GlslOp.NMax
    case InterpolateAtSample   => GlslOp.InterpolateAtSample
    case InterpolateAtOffset   => GlslOp.InterpolateAtOffset
    case FMix                  => GlslOp.FMix
    case SmoothStep            => GlslOp.SmoothStep
    case Fma                   => GlslOp.Fma
    case FaceForward           => GlslOp.FaceForward
    case Refract               => GlslOp.Refract
    case NClamp                => GlslOp.NClamp
    case Cross                 => GlslOp.Cross
    case Determinant           => GlslOp.Determinant
    case MatrixInverse         => GlslOp.MatrixInverse
    case PackSnorm4x8          => GlslOp.PackSnorm4x8
    case PackUnorm4x8          => GlslOp.PackUnorm4x8
    case PackSnorm2x16         => GlslOp.PackSnorm2x16
    case PackUnorm2x16         => GlslOp.PackUnorm2x16
    case PackHalf2x16          => GlslOp.PackHalf2x16

  private val findGlslSigned: PartialFunction[Operator, Code] =
    case Abs     => GlslOp.SAbs
    case Sign    => GlslOp.SSign
    case Min     => GlslOp.SMin
    case Max     => GlslOp.SMax
    case Clamp   => GlslOp.SClamp
    case FindLsb => GlslOp.FindILsb
    case FindMsb => GlslOp.FindSMsb

  private val findGlslUnsigned: PartialFunction[Operator, Code] =
    case Min             => GlslOp.UMin
    case Max             => GlslOp.UMax
    case Clamp           => GlslOp.UClamp
    case FindLsb         => GlslOp.FindILsb
    case FindMsb         => GlslOp.FindUMsb
    case UnpackSnorm2x16 => GlslOp.UnpackSnorm2x16
    case UnpackUnorm2x16 => GlslOp.UnpackUnorm2x16
    case UnpackHalf2x16  => GlslOp.UnpackHalf2x16
    case UnpackSnorm4x8  => GlslOp.UnpackSnorm4x8
    case UnpackUnorm4x8  => GlslOp.UnpackUnorm4x8
    case PackDouble2x32  => GlslOp.PackDouble2x32

object Algebra:
  final val GlslStd450 = "GLSL.std.450"
