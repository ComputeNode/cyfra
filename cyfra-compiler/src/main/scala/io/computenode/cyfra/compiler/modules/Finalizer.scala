package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.Compiler
import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.compiler.unit.Ctx
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.core.expression.{Value, given}
import io.computenode.cyfra.core.expression.BuildInFunction.GlobalInvocationId
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class Finalizer extends StandardCompilationModule:
  def compile(input: Compilation): Compilation =
    val main = input.functionBodies.last.body.head.asInstanceOf[RefIR[?]]
    val (prevPrefix, inputs) = input.context.prefix.partitionMap:
      case IR.Interface(ref) => Right(ref)
      case other             => Left(other)

    val config = input.metadata.config match
      case x: Compiler.Compute => x

    val prefix = List(
      IR.SvInst(Op.OpCapability, Capability.Shader :: Nil),
      IR.SvInst(Op.OpMemoryModel, AddressingModel.Logical :: MemoryModel.GLSL450 :: Nil),
      IR.SvInst(Op.OpEntryPoint, ExecutionModel.GLCompute :: main :: Text("main") :: inputs),
      IR.SvInst(Op.OpExecutionMode, main :: ExecutionMode.LocalSize :: config.workgroupSize.toList.map(IntWord.apply)),
      IR.SvInst(Op.OpSource, SourceLanguage.Unknown :: IntWord(364) :: Nil),
      IR.SvInst(Op.OpSourceExtension, Text("Scala 3") :: Nil),
    )

    val c2 = input.context.copy(prefix = prefix ++ prevPrefix)

    input.copy(context = c2)
