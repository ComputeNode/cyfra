package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.compiler.unit.Ctx
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.spirv.Opcodes.*
import io.computenode.cyfra.core.expression.{UInt32, Value, Vec3, given}
import io.computenode.cyfra.core.expression.BuildInFunction.GlobalInvocationId
import izumi.reflect.Tag

class Finalizer extends StandardCompilationModule:
  def compile(input: Compilation): Compilation =
    val main = input.functionBodies.last.body.head.asInstanceOf[RefIR[?]]

    val ((invocationVar, workgroupConst), c1) = Ctx.withCapability(input.context):
      val tpe = Ctx.getTypePointer(Value[Vec3[UInt32]], StorageClass.Input)
      val irv = IR.SvRef[Unit](Op.OpVariable, tpe :: StorageClass.Input :: Nil)
      val wgs = Ctx.getConstant[Vec3[UInt32]](256, 1, 1)
      (irv, wgs)

    val decorations = List(
      IR.SvInst(Op.OpDecorate, invocationVar :: Decoration.BuiltIn :: BuiltIn.GlobalInvocationId :: Nil),
      IR.SvInst(Op.OpDecorate, workgroupConst :: Decoration.BuiltIn :: BuiltIn.WorkgroupSize :: Nil),
    )

    val prefix = List(
      IR.SvInst(Op.OpCapability, Capability.Shader :: Nil),
      IR.SvInst(Op.OpMemoryModel, AddressingModel.Logical :: MemoryModel.GLSL450 :: Nil),
      IR.SvInst(Op.OpEntryPoint, ExecutionModel.GLCompute :: main :: Text("main") :: invocationVar :: Nil),
      IR.SvInst(Op.OpExecutionMode, main :: ExecutionMode.LocalSize :: IntWord(256) :: IntWord(1) :: IntWord(1) :: Nil),
      IR.SvInst(Op.OpSource, SourceLanguage.Unknown :: IntWord(364) :: Nil),
      IR.SvInst(Op.OpSourceExtension, Text("Scala 3") :: Nil),
    )

    val c2 = c1.copy(prefix = prefix, decorations = decorations ++ c1.decorations, suffix = invocationVar :: c1.suffix)

    val (mapped, c3) = Ctx.withCapability(c2):
      input.functionBodies.map: irs =>
        irs.flatMapReplace:
          case IR.Operation(GlobalInvocationId, Nil) =>
            val ptrX = Ctx.getTypePointer(Value[UInt32], StorageClass.Input)
            val zeroU = Ctx.getConstant[UInt32](0)
            val tpe = Ctx.getType(Value[UInt32])
            val accessChain = IR.SvRef[Unit](Op.OpAccessChain, ptrX :: invocationVar :: zeroU :: Nil)
            val ir = IR.SvRef[UInt32](Op.OpLoad, tpe :: accessChain :: Nil)
            IRs(ir, List(accessChain, ir))
          case other => IRs(other)(using other.v)

    input.copy(context = c3, functionBodies = mapped)
