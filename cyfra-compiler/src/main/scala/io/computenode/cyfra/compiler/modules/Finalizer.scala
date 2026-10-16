package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.{CompilationException, Compiler}
import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.CompilationUnit
import io.computenode.cyfra.compiler.unit.Ctx
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.core.expression.{Value, given}
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class Finalizer extends StandardCompilationModule:
  def compile(input: CompilationUnit): CompilationUnit =
    val main = input.functionBodies.last.body.head.asInstanceOf[RefIR[?]]
    val (prevPrefix, inputs) = input.context.prefix.partitionMap:
      case IR.Interface(ref) => Right(ref)
      case other             => Left(other)

    val config = input.metadata.config match
      case x: Compiler.Compute => x

    val prefix = List(
      IR.SvInst(Op.OpMemoryModel, AddressingModel.Logical :: MemoryModel.GLSL450 :: Nil),
      IR.SvInst(Op.OpEntryPoint, ExecutionModel.GLCompute :: main :: Text("main") :: inputs),
      IR.SvInst(Op.OpExecutionMode, main :: ExecutionMode.LocalSize :: config.workgroupSize.toList.map(IntWord.apply)),
      IR.SvInst(Op.OpSource, SourceLanguage.Unknown :: IntWord(364) :: Nil),
      IR.SvInst(Op.OpSourceExtension, Text("Scala 3") :: Nil),
    )

    val cap = input.metadata.config match
      case _: Compiler.Compute => Capability.Shader

    val (_, context) = Ctx.withCapability(input.context):
      Ctx.enableCapability(cap)
    val declarations = orderDeclarations(context.types.block.reverse ++ context.constants.output)
    // Finalization consumes declaration blocks; their caches and annotations remain intact.
    val finalized = context.copy(
      prefix = prefix ++ prevPrefix,
      types = context.types.copy(block = Nil),
      constants = context.constants.copy(block = Nil),
      suffix = declarations ++ context.suffix,
    )
    input.copy(context = finalized)

  private def orderDeclarations(declarations: List[IR[?]]): List[IR[?]] =
    val refs = declarations.map:
      case ref: IR.SvRef[?] => ref
      case other            => throw CompilationException(s"Expected a type or constant declaration, got ${other.name}")
    val byId = refs.map(ref => ref.id -> ref).toMap

    def visit(ref: IR.SvRef[?], visiting: Set[Int], state: (Set[Int], List[IR[?]])): (Set[Int], List[IR[?]]) =
      if state._1(ref.id) then return state
      if visiting(ref.id) then throw CompilationException(s"Cyclic type/constant dependency at ${ref.name} #${ref.id}")
      val dependencies = ref.tpe.toList ++ ref.operands.collect { case dependency: RefIR[?] => dependency }
      val (visited, ordered) = dependencies.foldLeft(state): (acc, dependency) =>
        byId.get(dependency.id).fold(acc)(visit(_, visiting + ref.id, acc))
      (visited + ref.id, ref :: ordered)

    refs.foldLeft((Set.empty[Int], List.empty[IR[?]]))((state, ref) => visit(ref, Set.empty, state))._2.reverse
