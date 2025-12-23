package io.computenode.cyfra.spirv.archive

import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.macros.FnCall.FnIdentifier
import SpirvConstants.HEADER_REFS_TOP
import io.computenode.cyfra.spirv.archive.compilers.FunctionCompiler.SprivFunction
import io.computenode.cyfra.spirv.archive.compilers.SpirvProgramCompiler.ArrayBufferBlock
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

private[cyfra] case class Context(
  valueTypeMap: Map[LightTypeTag, Int] = Map(),
  funPointerTypeMap: Map[Int, Int] = Map(),
  uniformPointerMap: Map[Int, Int] = Map(),
  inputPointerMap: Map[Int, Int] = Map(),
  funcTypeMap: Map[(LightTypeTag, List[LightTypeTag]), Int] = Map(),
  voidTypeRef: Int = -1,
  voidFuncTypeRef: Int = -1,
  workerIndexRef: Int = -1,
  uniformVarRefs: Map[GUniform[?], Int] = Map.empty,
  bindingToStructType: Map[Int, Int] = Map.empty,
  constRefs: Map[(Tag[?], Any), Int] = Map(),
  exprRefs: Map[Int, Int] = Map(),
  bufferBlocks: Map[GBuffer[?], ArrayBufferBlock] = Map(),
  nextResultId: Int = HEADER_REFS_TOP,
  nextBinding: Int = 0,
  exprNames: Map[Int, String] = Map(),
  names: Set[String] = Set(),
  functions: Map[FnIdentifier, SprivFunction] = Map(),
  stringLiterals: Map[String, Int] = Map(),
):
  def joinNested(ctx: Context): Context =
    this.copy(nextResultId = ctx.nextResultId, exprNames = ctx.exprNames ++ this.exprNames, functions = ctx.functions ++ this.functions)

private[cyfra] object Context:

  def initialContext: Context = Context()
