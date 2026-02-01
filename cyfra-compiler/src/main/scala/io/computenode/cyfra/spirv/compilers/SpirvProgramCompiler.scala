package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.dsl.Expression.{Const, E}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStructConstructor, GStructSchema}
import io.computenode.cyfra.dsl.struct.GStruct.GetField
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.SpirvConstants.*
import io.computenode.cyfra.spirv.SpirvTypes.*
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.compileBlock
import izumi.reflect.Tag

private[cyfra] object SpirvProgramCompiler:

  def bubbleUpVars(exprs: List[Words]): (List[Words], List[Words]) =
    exprs.partition:
      case Instruction(Op.OpVariable, _) => true
      case _                             => false

  def compileMain(bodyIo: GIO[?], ctx: Context): (List[Words], Context) =
    val int32TypeRef = ctx.valueTypeMap(Int32Tag.tag)
    val vec3Int32TypeRef = ctx.valueTypeMap(summon[Tag[Vec3[Int32]]].tag)
    val int32PtrInputRef = ctx.inputPointerMap(int32TypeRef)
    val vec3Int32PtrInputRef = ctx.inputPointerMap(vec3Int32TypeRef)
    val zeroConstRef = ctx.constRefs(Int32Tag, 0)

    val init = List(
      Instruction(Op.OpFunction, List(ResultRef(ctx.voidTypeRef), ResultRef(MAIN_FUNC_REF), SamplerAddressingMode.None, ResultRef(VOID_FUNC_TYPE_REF))),
      Instruction(Op.OpLabel, List(ResultRef(ctx.nextResultId))),
    )

    var nextId = ctx.nextResultId + 1

    def loadScalarFromVec3(varRef: Int): (List[Words], Int) =
      val ptrId = nextId
      val loadId = nextId + 1
      nextId += 2
      val insns = List(
        Instruction(Op.OpAccessChain, List(ResultRef(int32PtrInputRef), ResultRef(ptrId), ResultRef(varRef), ResultRef(zeroConstRef))),
        Instruction(Op.OpLoad, List(ResultRef(int32TypeRef), ResultRef(loadId), ResultRef(ptrId))),
      )
      (insns, loadId)

    def loadVec3(varRef: Int): (List[Words], Int) =
      val loadId = nextId
      nextId += 1
      val insns = List(Instruction(Op.OpLoad, List(ResultRef(vec3Int32TypeRef), ResultRef(loadId), ResultRef(varRef))))
      (insns, loadId)

    def loadScalar(varRef: Int): (List[Words], Int) =
      val loadId = nextId
      nextId += 1
      val insns = List(Instruction(Op.OpLoad, List(ResultRef(int32TypeRef), ResultRef(loadId), ResultRef(varRef))))
      (insns, loadId)

    val (globalInvocInsns, globalInvocId) = loadScalarFromVec3(GL_GLOBAL_INVOCATION_ID_REF)
    val (localIdInsns, localIdRef) = loadVec3(GL_LOCAL_INVOCATION_ID_REF)
    val (localIndexInsns, localIndexRef) = loadScalar(GL_LOCAL_INVOCATION_INDEX_REF)
    val (workgroupIdInsns, workgroupIdLoadRef) = loadVec3(GL_WORKGROUP_ID_REF)
    val (numWorkgroupsInsns, numWorkgroupsLoadRef) = loadVec3(GL_NUM_WORKGROUPS_REF)
    val (subgroupIdInsns, subgroupIdLoadRef) = loadScalar(GL_SUBGROUP_ID_REF)
    val (subgroupLocalIdInsns, subgroupLocalIdLoadRef) = loadScalar(GL_SUBGROUP_LOCAL_INVOCATION_ID_REF)
    val (subgroupSizeInsns, subgroupSizeLoadRef) = loadScalar(GL_SUBGROUP_SIZE_REF)

    val loadInsns = globalInvocInsns ::: localIdInsns ::: localIndexInsns :::
      workgroupIdInsns ::: numWorkgroupsInsns :::
      subgroupIdInsns ::: subgroupLocalIdInsns ::: subgroupSizeInsns

    val bodyCtx = ctx.copy(
      nextResultId = nextId,
      workerIndexRef = globalInvocId,
      localInvocationIdRef = localIdRef,
      localInvocationIndexRef = localIndexRef,
      workgroupIdRef = workgroupIdLoadRef,
      numWorkgroupsRef = numWorkgroupsLoadRef,
      subgroupIdRef = subgroupIdLoadRef,
      subgroupLocalInvocationIdRef = subgroupLocalIdLoadRef,
      subgroupSizeRef = subgroupSizeLoadRef,
    )

    val (body, codeCtx) = GIOCompiler.compileGio(bodyIo, bodyCtx)

    val (vars, nonVarsBody) = bubbleUpVars(body)

    val end = List(Instruction(Op.OpReturn, List()), Instruction(Op.OpFunctionEnd, List()))
    (init ::: vars ::: loadInsns ::: nonVarsBody ::: end, codeCtx.copy(nextResultId = codeCtx.nextResultId + 1))

  def getNameDecorations(ctx: Context): List[Instruction] =
    val funNames = ctx.functions.map { case (id, fn) =>
      (fn.functionId, fn.sourceFn.fullName)
    }.toList
    val allNames = ctx.exprNames ++ funNames
    allNames.map { case (id, name) =>
      Instruction(Op.OpName, List(ResultRef(id), Text(name)))
    }.toList

  case class ArrayBufferBlock(
    structTypeRef: Int, // %BufferX
    blockVarRef: Int, // %__X
    blockPointerRef: Int, // _ptr_Uniform_OutputBufferX
    memberArrayTypeRef: Int, // %_runtimearr_float_X
    binding: Int,
  )

  case class SharedBlock(
    arrayTypeRef: Int,
    varRef: Int,
    pointerTypeRef: Int,
  )

  def headers(workgroupSize: (Int, Int, Int)): List[Words] =
    val (localSizeX, localSizeY, localSizeZ) = workgroupSize
    Word(Array(0x03, 0x02, 0x23, 0x07)) :: // SPIR-V
      Word(Array(0x00, 0x03, 0x01, 0x00)) :: // Version: 1.3.0 (for GroupNonUniform)
      Word(Array(cyfraVendorId, 0x00, 0x01, 0x00)) :: // Generator: cyfra; 1
      WordVariable(BOUND_VARIABLE) :: // Bound: To be calculated
      Word(Array(0x00, 0x00, 0x00, 0x00)) :: // Schema: 0
      Instruction(Op.OpCapability, List(Capability.Shader)) ::
      Instruction(Op.OpCapability, List(Capability.Float16)) ::
      Instruction(Op.OpCapability, List(Capability.GroupNonUniform)) ::
      Instruction(Op.OpCapability, List(Capability.GroupNonUniformArithmetic)) ::
      Instruction(Op.OpExtension, List(Text("SPV_KHR_non_semantic_info"))) ::
      Instruction(Op.OpExtInstImport, List(ResultRef(GLSL_EXT_REF), Text(GLSL_EXT_NAME))) ::
      Instruction(Op.OpExtInstImport, List(ResultRef(DEBUG_PRINTF_REF), Text(NON_SEMANTIC_DEBUG_PRINTF))) ::
      Instruction(Op.OpMemoryModel, List(AddressingModel.Logical, MemoryModel.GLSL450)) ::
      Instruction(
        Op.OpEntryPoint,
        List(
          ExecutionModel.GLCompute,
          ResultRef(MAIN_FUNC_REF),
          Text("main"),
          ResultRef(GL_GLOBAL_INVOCATION_ID_REF),
          ResultRef(GL_LOCAL_INVOCATION_ID_REF),
          ResultRef(GL_LOCAL_INVOCATION_INDEX_REF),
          ResultRef(GL_WORKGROUP_ID_REF),
          ResultRef(GL_NUM_WORKGROUPS_REF),
          ResultRef(GL_SUBGROUP_ID_REF),
          ResultRef(GL_SUBGROUP_LOCAL_INVOCATION_ID_REF),
          ResultRef(GL_SUBGROUP_SIZE_REF),
        ),
      ) ::
      Instruction(Op.OpExecutionMode, List(ResultRef(MAIN_FUNC_REF), ExecutionMode.LocalSize, IntWord(localSizeX), IntWord(localSizeY), IntWord(localSizeZ))) ::
      Instruction(Op.OpSource, List(SourceLanguage.GLSL, IntWord(450))) ::
      Nil

  val workgroupDecorations: List[Words] =
    List(
      Instruction(Op.OpDecorate, List(ResultRef(GL_GLOBAL_INVOCATION_ID_REF), Decoration.BuiltIn, BuiltIn.GlobalInvocationId)),
      Instruction(Op.OpDecorate, List(ResultRef(GL_WORKGROUP_SIZE_REF), Decoration.BuiltIn, BuiltIn.WorkgroupSize)),
      Instruction(Op.OpDecorate, List(ResultRef(GL_LOCAL_INVOCATION_ID_REF), Decoration.BuiltIn, BuiltIn.LocalInvocationId)),
      Instruction(Op.OpDecorate, List(ResultRef(GL_LOCAL_INVOCATION_INDEX_REF), Decoration.BuiltIn, BuiltIn.LocalInvocationIndex)),
      Instruction(Op.OpDecorate, List(ResultRef(GL_WORKGROUP_ID_REF), Decoration.BuiltIn, BuiltIn.WorkgroupId)),
      Instruction(Op.OpDecorate, List(ResultRef(GL_NUM_WORKGROUPS_REF), Decoration.BuiltIn, BuiltIn.NumWorkgroups)),
      Instruction(Op.OpDecorate, List(ResultRef(GL_SUBGROUP_ID_REF), Decoration.BuiltIn, BuiltIn.SubgroupId)),
      Instruction(Op.OpDecorate, List(ResultRef(GL_SUBGROUP_LOCAL_INVOCATION_ID_REF), Decoration.BuiltIn, BuiltIn.SubgroupLocalInvocationId)),
      Instruction(Op.OpDecorate, List(ResultRef(GL_SUBGROUP_SIZE_REF), Decoration.BuiltIn, BuiltIn.SubgroupSize)),
    )

  def defineVoids(context: Context): (List[Words], Context) =
    val voidDef = List[Words](
      Instruction(Op.OpTypeVoid, List(ResultRef(TYPE_VOID_REF))),
      Instruction(Op.OpTypeFunction, List(ResultRef(VOID_FUNC_TYPE_REF), ResultRef(TYPE_VOID_REF))),
    )
    val ctxWithVoid = context.copy(voidTypeRef = TYPE_VOID_REF, voidFuncTypeRef = VOID_FUNC_TYPE_REF)
    (voidDef, ctxWithVoid)

  def createInvocationId(context: Context, workgroupSize: (Int, Int, Int)): (List[Words], Context) =
    val (localSizeX, localSizeY, localSizeZ) = workgroupSize
    val definitionInstructions = List(
      Instruction(Op.OpConstant, List(ResultRef(context.valueTypeMap(UInt32Tag.tag)), ResultRef(context.nextResultId + 0), IntWord(localSizeX))),
      Instruction(Op.OpConstant, List(ResultRef(context.valueTypeMap(UInt32Tag.tag)), ResultRef(context.nextResultId + 1), IntWord(localSizeY))),
      Instruction(Op.OpConstant, List(ResultRef(context.valueTypeMap(UInt32Tag.tag)), ResultRef(context.nextResultId + 2), IntWord(localSizeZ))),
      Instruction(
        Op.OpConstantComposite,
        List(
          IntWord(context.valueTypeMap(summon[Tag[Vec3[UInt32]]].tag)),
          ResultRef(GL_WORKGROUP_SIZE_REF),
          ResultRef(context.nextResultId + 0),
          ResultRef(context.nextResultId + 1),
          ResultRef(context.nextResultId + 2),
        ),
      ),
    )
    (definitionInstructions, context.copy(nextResultId = context.nextResultId + 3))
  def initAndDecorateBuffers(buffers: List[(GBuffer[?], Int)], context: Context): (List[Words], List[Words], Context) =
    val (blockDecor, blockDef, inCtx) = createAndInitBlocks(buffers, context)
    val (voidsDef, voidCtx) = defineVoids(inCtx)
    (blockDecor, voidsDef ::: blockDef, voidCtx)

  def createAndInitBlocks(blocks: List[(GBuffer[?], Int)], context: Context): (List[Words], List[Words], Context) =
    var membersVisited = Set[Int]()
    var structsVisited = Set[Int]()
    val (decoration, definition, newContext) = blocks.foldLeft((List[Words](), List[Words](), context)) {
      case ((decAcc, insnAcc, ctx), (buff, binding)) =>
        val tpe = buff.tag
        val block = ArrayBufferBlock(ctx.nextResultId, ctx.nextResultId + 1, ctx.nextResultId + 2, ctx.nextResultId + 3, binding)

        val (structDecoration, structDefinition) =
          if structsVisited.contains(block.structTypeRef) then (Nil, Nil)
          else
            structsVisited += block.structTypeRef
            (
              List(
                Instruction(Op.OpMemberDecorate, List(ResultRef(block.structTypeRef), IntWord(0), Decoration.Offset, IntWord(0))), // OpMemberDecorate %BufferX 0 Offset 0
                Instruction(Op.OpDecorate, List(ResultRef(block.structTypeRef), Decoration.BufferBlock)), // OpDecorate %BufferX BufferBlock
              ),
              List(
                Instruction(Op.OpTypeStruct, List(ResultRef(block.structTypeRef), IntWord(block.memberArrayTypeRef))), // %BufferX = OpTypeStruct %_runtimearr_X
              ),
            )

        val (memberDecoration, memberDefinition) =
          if membersVisited.contains(block.memberArrayTypeRef) then (Nil, Nil)
          else
            membersVisited += block.memberArrayTypeRef
            (
              List(
                Instruction(Op.OpDecorate, List(ResultRef(block.memberArrayTypeRef), Decoration.ArrayStride, IntWord(typeStride(tpe)))), // OpDecorate %_runtimearr_X ArrayStride [typeStride(type)]
              ),
              List(
                Instruction(Op.OpTypeRuntimeArray, List(ResultRef(block.memberArrayTypeRef), IntWord(context.valueTypeMap(tpe.tag)))), // %_runtimearr_X = OpTypeRuntimeArray %[typeOf(tpe)]
              ),
            )

        val decorationInstructions = memberDecoration ::: structDecoration ::: List[Words](
          Instruction(Op.OpDecorate, List(ResultRef(block.blockVarRef), Decoration.DescriptorSet, IntWord(0))), // OpDecorate %_X DescriptorSet 0
          Instruction(Op.OpDecorate, List(ResultRef(block.blockVarRef), Decoration.Binding, IntWord(block.binding))), // OpDecorate %_X Binding [binding]
        )

        val definitionInstructions = memberDefinition ::: structDefinition ::: List[Words](
          Instruction(Op.OpTypePointer, List(ResultRef(block.blockPointerRef), StorageClass.Uniform, ResultRef(block.structTypeRef))), // %_ptr_Uniform_BufferX= OpTypePointer Uniform %BufferX
          Instruction(Op.OpVariable, List(ResultRef(block.blockPointerRef), ResultRef(block.blockVarRef), StorageClass.Uniform)), // %_X = OpVariable %_ptr_Uniform_X Uniform
        )

        val contextWithBlock =
          ctx.copy(bufferBlocks = ctx.bufferBlocks + (buff -> block))
        (decAcc ::: decorationInstructions, insnAcc ::: definitionInstructions, contextWithBlock.copy(nextResultId = contextWithBlock.nextResultId + 5))
    }
    (decoration, definition, newContext)

  def getBlockNames(context: Context, uniformSchemas: List[GUniform[?]]): List[Words] =
    def namesForBlock(block: ArrayBufferBlock, tpe: String): List[Words] =
      Instruction(Op.OpName, List(ResultRef(block.structTypeRef), Text(s"Buffer$tpe"))) ::
        Instruction(Op.OpName, List(ResultRef(block.blockVarRef), Text(s"data$tpe"))) :: Nil
    // todo name uniform
    // context.inBufferBlocks.flatMap(namesForBlock(_, "In")) ::: context.outBufferBlocks.flatMap(namesForBlock(_, "Out"))
    List()

  def totalStride(gs: GStructSchema[?]): Int = gs.fields
    .map:
      case (_, fromExpr, t) if t <:< gs.gStructTag =>
        val constructor = fromExpr.asInstanceOf[GStructConstructor[?]]
        totalStride(constructor.schema)
      case (_, _, t) =>
        typeStride(t)
    .sum

  def defineStrings(strings: List[String], ctx: Context): (List[Words], Context) =
    strings.foldLeft((List.empty[Words], ctx)):
      case ((insnsAcc, currentCtx), str) =>
        if currentCtx.stringLiterals.contains(str) then (insnsAcc, currentCtx)
        else
          val strRef = currentCtx.nextResultId
          val strInsns = List(Instruction(Op.OpString, List(ResultRef(strRef), Text(str))))
          val newCtx = currentCtx.copy(stringLiterals = currentCtx.stringLiterals + (str -> strRef), nextResultId = currentCtx.nextResultId + 1)
          (insnsAcc ::: strInsns, newCtx)

  def createAndInitUniformBlocks(schemas: List[(GUniform[?], Int)], ctx: Context): (List[Words], List[Words], Context) = {
    var decoratedOffsets = Set[Int]()
    schemas.foldLeft((List.empty[Words], List.empty[Words], ctx)) { case ((decorationsAcc, definitionsAcc, currentCtx), (uniform, binding)) =>
      val schema = uniform.schema
      val uniformStructTypeRef = currentCtx.valueTypeMap(schema.structTag.tag)

      val structDecorations =
        if decoratedOffsets.contains(uniformStructTypeRef) then Nil
        else
          decoratedOffsets += uniformStructTypeRef
          schema.fields.zipWithIndex
            .foldLeft[(List[Words], Int)](List.empty[Words], 0):
              case ((acc, offset), ((name, fromExpr, tag), idx)) =>
                val stride =
                  if tag <:< schema.gStructTag then
                    val constructor = fromExpr.asInstanceOf[GStructConstructor[?]]
                    totalStride(constructor.schema)
                  else typeStride(tag)
                val offsetDecoration =
                  Instruction(Op.OpMemberDecorate, List(ResultRef(uniformStructTypeRef), IntWord(idx), Decoration.Offset, IntWord(offset)))
                (acc :+ offsetDecoration, offset + stride)
            ._1 ::: List(Instruction(Op.OpDecorate, List(ResultRef(uniformStructTypeRef), Decoration.Block)))

      val uniformPointerUniformRef = currentCtx.nextResultId
      val uniformPointerUniform =
        Instruction(Op.OpTypePointer, List(ResultRef(uniformPointerUniformRef), StorageClass.Uniform, ResultRef(uniformStructTypeRef)))

      val uniformVarRef = currentCtx.nextResultId + 1
      val uniformVar = Instruction(Op.OpVariable, List(ResultRef(uniformPointerUniformRef), ResultRef(uniformVarRef), StorageClass.Uniform))

      val uniformDecorateDescriptorSet = Instruction(Op.OpDecorate, List(ResultRef(uniformVarRef), Decoration.DescriptorSet, IntWord(0)))
      val uniformDecorateBinding = Instruction(Op.OpDecorate, List(ResultRef(uniformVarRef), Decoration.Binding, IntWord(binding)))

      val newDecorations = decorationsAcc ::: structDecorations ::: List(uniformDecorateDescriptorSet, uniformDecorateBinding)
      val newDefinitions = definitionsAcc ::: List(uniformPointerUniform, uniformVar)
      val newCtx = currentCtx.copy(
        nextResultId = currentCtx.nextResultId + 2,
        uniformVarRefs = currentCtx.uniformVarRefs + (uniform -> uniformVarRef),
        uniformPointerMap = currentCtx.uniformPointerMap + (uniformStructTypeRef -> uniformPointerUniformRef),
        bindingToStructType = currentCtx.bindingToStructType + (binding -> uniformStructTypeRef),
      )

      (newDecorations, newDefinitions, newCtx)
    }
  }

  val predefinedConsts = List(
    (Int32Tag, 0),
    (UInt32Tag, 0),
    (Int32Tag, 1),
    (Int32Tag, Scope.Workgroup.opcode),
    (Int32Tag, Scope.Subgroup.opcode),
    (Int32Tag, MemorySemantics.WorkgroupMemory.opcode | MemorySemantics.AcquireRelease.opcode),
  )
  def defineConstants(exprs: List[E[?]], ctx: Context): (List[Words], Context) =
    // Collect field indices from GetField expressions
    val fieldIndices = exprs.collect { case gf: GetField[?, ?] =>
      (Int32Tag, gf.fieldIndex)
    }.distinct

    val consts =
      (exprs.collect { case c @ Const(x) =>
        (c.tag, x)
      } ::: predefinedConsts ::: fieldIndices).distinct.filterNot(_._1 == GBooleanTag)
    val (insns, newC) = consts.foldLeft((List[Words](), ctx)) { case ((instructions, context), const) =>
      val insn =
        Instruction(Op.OpConstant, List(ResultRef(context.valueTypeMap(const._1.tag)), ResultRef(context.nextResultId), toWord(const._1, const._2)))
      val ctx = context.copy(constRefs = context.constRefs + (const -> context.nextResultId), nextResultId = context.nextResultId + 1)
      (instructions :+ insn, ctx)
    }
    val withBool = insns ::: List(
      Instruction(Op.OpConstantTrue, List(ResultRef(ctx.valueTypeMap(GBooleanTag.tag)), ResultRef(newC.nextResultId))),
      Instruction(Op.OpConstantFalse, List(ResultRef(ctx.valueTypeMap(GBooleanTag.tag)), ResultRef(newC.nextResultId + 1))),
    )
    (
      withBool,
      newC.copy(
        nextResultId = newC.nextResultId + 2,
        constRefs = newC.constRefs ++ Map((GBooleanTag, true) -> newC.nextResultId, (GBooleanTag, false) -> (newC.nextResultId + 1)),
      ),
    )

  def defineVarNames(ctx: Context): (List[Words], Context) =
    val vec3Int32PtrId = ctx.inputPointerMap(ctx.valueTypeMap(summon[Tag[Vec3[Int32]]].tag))
    val int32PtrInputId = ctx.inputPointerMap(ctx.valueTypeMap(summon[Tag[Int32]].tag))
    (
      List(
        Instruction(Op.OpVariable, List(ResultRef(vec3Int32PtrId), ResultRef(GL_GLOBAL_INVOCATION_ID_REF), StorageClass.Input)),
        Instruction(Op.OpVariable, List(ResultRef(vec3Int32PtrId), ResultRef(GL_LOCAL_INVOCATION_ID_REF), StorageClass.Input)),
        Instruction(Op.OpVariable, List(ResultRef(int32PtrInputId), ResultRef(GL_LOCAL_INVOCATION_INDEX_REF), StorageClass.Input)),
        Instruction(Op.OpVariable, List(ResultRef(vec3Int32PtrId), ResultRef(GL_WORKGROUP_ID_REF), StorageClass.Input)),
        Instruction(Op.OpVariable, List(ResultRef(vec3Int32PtrId), ResultRef(GL_NUM_WORKGROUPS_REF), StorageClass.Input)),
        Instruction(Op.OpVariable, List(ResultRef(int32PtrInputId), ResultRef(GL_SUBGROUP_ID_REF), StorageClass.Input)),
        Instruction(Op.OpVariable, List(ResultRef(int32PtrInputId), ResultRef(GL_SUBGROUP_LOCAL_INVOCATION_ID_REF), StorageClass.Input)),
        Instruction(Op.OpVariable, List(ResultRef(int32PtrInputId), ResultRef(GL_SUBGROUP_SIZE_REF), StorageClass.Input)),
      ),
      ctx,
    )
