package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.{Context, Ctx}
import io.computenode.cyfra.compiler.Spriv.Op
import io.computenode.cyfra.compiler.Spriv.Code
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.BuildInFunction.*
import izumi.reflect.Tag

class Algebra extends FunctionCompilationModule:
  def compileFunction(input: IRs[?])(using Ctx): IRs[?] =
    input.flatMapReplace:
      case x @ IR.Operation(GlobalInvocationId, _) => IRs(x)(using x.v)
      case x: IR.Operation[a] => handleOperation[a](x)(using x.v)
      case other              => IRs(other)(using other.v)

  private def handleOperation[A: Value](operation: IR.Operation[A])(using Ctx): IRs[A] =
    val IR.Operation(func, args) = operation
    val argBaseValue = args.head.v.bottomComposite
    val opCode = argBaseValue.tag match
      case t if t <:< Tag[FloatType]       => findFloat(func)
      case t if t <:< Tag[SignedIntType]   => findInteger(func, true)
      case t if t <:< Tag[UnsignedIntType] => findInteger(func, false)
      case t if t =:= Tag[Bool]            => findBoolean(func)
      case t if t =:= Tag[Unit]            => return IRs(operation) // skip invocation id

    val tpe = Ctx.getType(Value[A])
    IRs(IR.SvRef[A](opCode, tpe , args))

  private def findFloat(func: BuildInFunction[?]): Code =
    func match
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

      case other => throw CompilationException(s"$func for Float type not found")

  private def findBoolean(func: BuildInFunction[?]): Code =
    func match
      case LogicalAny      => Op.OpAny
      case LogicalAll      => Op.OpAll
      case LogicalEqual    => Op.OpLogicalEqual
      case LogicalNotEqual => Op.OpLogicalNotEqual
      case LogicalOr       => Op.OpLogicalOr
      case LogicalAnd      => Op.OpLogicalAnd
      case LogicalNot      => Op.OpLogicalNot

      case Select => Op.OpSelect // This code need more research
      case other  => throw CompilationException(s"$func for Bool type not found")

  private def findInteger(func: BuildInFunction[?], signed: Boolean): Code =
    func match
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
      case other    => if signed then findSignedInteger(other) else findUnsignedInteger(other)

  private def findSignedInteger(func: BuildInFunction[?]): Code =
    func match
      case Div => Op.OpSDiv
      case Mod => Op.OpSMod

      case Neg => Op.OpSNegate
      case Rem => Op.OpSRem

      case BitFieldExtract => Op.OpBitFieldSExtract

      case LessThan         => Op.OpSLessThan
      case GreaterThan      => Op.OpSGreaterThan
      case LessThanEqual    => Op.OpSLessThanEqual
      case GreaterThanEqual => Op.OpSGreaterThanEqual

      case other => throw CompilationException(s"$func for SInt type not found")

  private def findUnsignedInteger(func: BuildInFunction[?]): Code =
    func match
      case Div => Op.OpUDiv
      case Mod => Op.OpUMod

      case BitFieldExtract => Op.OpBitFieldUExtract

      case LessThan         => Op.OpULessThan
      case GreaterThan      => Op.OpUGreaterThan
      case LessThanEqual    => Op.OpULessThanEqual
      case GreaterThanEqual => Op.OpUGreaterThanEqual

      case other => throw CompilationException(s"$func for UInt type not found")
