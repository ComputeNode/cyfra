package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.Value.Scalar
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GShared, GUniform, ReadShared, WriteBuffer, WriteShared, WriteUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.*
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.spirv.SpirvConstants.*
import io.computenode.cyfra.spirv.SpirvTypes.*
import io.computenode.cyfra.spirv.compilers.FunctionCompiler.compileFunctions
import io.computenode.cyfra.spirv.compilers.GStructCompiler.*
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.*
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.collection.mutable
import scala.runtime.stdLibPatches.Predef.summon

private[cyfra] object DSLCompiler:

  @tailrec
  private def getAllExprsFlattened(pending: List[GIO[?]], acc: List[E[?]], visitDetached: Boolean): List[E[?]] =
    pending match
      case Nil                 => acc
      case GIO.Pure(v) :: tail =>
        getAllExprsFlattened(tail, getAllExprsFlattened(v.tree, visitDetached) ::: acc, visitDetached)
      case GIO.FlatMap(v, n) :: tail =>
        getAllExprsFlattened(v :: n :: tail, acc, visitDetached)
      case GIO.Repeat(n, gio, _) :: tail =>
        val nAllExprs = getAllExprsFlattened(n.tree, visitDetached)
        getAllExprsFlattened(gio :: tail, nAllExprs ::: acc, visitDetached)
      case WriteBuffer(_, index, value) :: tail =>
        val indexAllExprs = getAllExprsFlattened(index.tree, visitDetached)
        val valueAllExprs = getAllExprsFlattened(value.tree, visitDetached)
        getAllExprsFlattened(tail, indexAllExprs ::: valueAllExprs ::: acc, visitDetached)
      case WriteUniform(_, value) :: tail =>
        val valueAllExprs = getAllExprsFlattened(value.tree, visitDetached)
        getAllExprsFlattened(tail, valueAllExprs ::: acc, visitDetached)
      case GIO.Printf(_, args*) :: tail =>
        val argsAllExprs = args.flatMap(a => getAllExprsFlattened(a.tree, visitDetached)).toList
        getAllExprsFlattened(tail, argsAllExprs ::: acc, visitDetached)
      case GIO.WorkgroupBarrier :: tail =>
        getAllExprsFlattened(tail, acc, visitDetached)
      case WriteShared(_, index, value) :: tail =>
        val indexAllExprs = getAllExprsFlattened(index.tree, visitDetached)
        val valueAllExprs = getAllExprsFlattened(value.tree, visitDetached)
        getAllExprsFlattened(tail, indexAllExprs ::: valueAllExprs ::: acc, visitDetached)
      case GIO.FoldRepeat(n, init, body, _, _) :: tail =>
        val nAllExprs = getAllExprsFlattened(n.tree, visitDetached)
        val initAllExprs = getAllExprsFlattened(init.tree, visitDetached)
        getAllExprsFlattened(body :: tail, nAllExprs ::: initAllExprs ::: acc, visitDetached)

  // TODO: Not traverse same fn scopes for each fn call
  private def getAllExprsFlattened(root: E[?], visitDetached: Boolean): List[E[?]] =
    var blockI = 0
    val allScopesCache = mutable.Map[Int, List[E[?]]]()
    val visited = mutable.Set[Int]()
    @tailrec
    def getAllScopesExprsAcc(toVisit: List[E[?]], acc: List[E[?]] = Nil): List[E[?]] = toVisit match
      case Nil                                     => acc
      case e :: tail if visited.contains(e.treeid) => getAllScopesExprsAcc(tail, acc)
      case e :: tail                               => // todo i don't think this really works (tail not used???)
        if allScopesCache.contains(root.treeid) then return allScopesCache(root.treeid)
        val eScopes = e.introducedScopes
        val filteredScopes = if visitDetached then eScopes else eScopes.filterNot(_.isDetached)
        val newToVisit = toVisit ::: e.exprDependencies ::: filteredScopes.map(_.expr)
        val result = e.exprDependencies ::: filteredScopes.map(_.expr) ::: acc
        visited += e.treeid
        blockI += 1
        if blockI % 100 == 0 then allScopesCache.update(e.treeid, result)
        getAllScopesExprsAcc(newToVisit, result)
    val result = root :: getAllScopesExprsAcc(root :: Nil)
    allScopesCache(root.treeid) = result
    result

  private def getAllShared(pending: List[GIO[?]], acc: Map[Int, GShared[?]]): Map[Int, GShared[?]] =
    pending match
      case Nil => acc
      case GIO.FlatMap(v, n) :: tail =>
        getAllShared(v :: n :: tail, acc)
      case GIO.Repeat(_, gio, _) :: tail =>
        getAllShared(gio :: tail, acc)
      case GIO.FoldRepeat(_, _, gio, _, _) :: tail =>
        getAllShared(gio :: tail, acc)
      case WriteShared(buffer, _, _) :: tail =>
        val impl = buffer.asInstanceOf[GShared.GSharedImpl[?]]
        getAllShared(tail, acc + (impl.sharedId -> buffer))
      case _ :: tail => getAllShared(tail, acc)

  private def getAllSharedFromExprs(exprs: List[E[?]], acc: Map[Int, GShared[?]]): Map[Int, GShared[?]] =
    exprs.foldLeft(acc):
      case (a, ReadShared(buffer, _)) =>
        val impl = buffer.asInstanceOf[GShared.GSharedImpl[?]]
        a + (impl.sharedId -> buffer)
      case (a, _) => a

  private def createSharedVariables(sharedBuffers: Map[Int, GShared[?]], ctx: Context): (List[Words], Context) =
    sharedBuffers.foldLeft((List.empty[Words], ctx)):
      case ((insnsAcc, currentCtx), (sharedId, buffer)) =>
        val elementTypeRef = currentCtx.valueTypeMap(buffer.tag.tag)
        val arraySizeConstRef = currentCtx.constRefs.getOrElse(
          (Int32Tag, buffer.size),
          throw new IllegalStateException(s"Missing constant for shared array size ${buffer.size}"),
        )

        // SPIR-V shared memory structure:
        // 1. Array type: OpTypeArray %arrayType %elementType %size
        // 2. Pointer to array: OpTypePointer %ptrArrayType Workgroup %arrayType
        // 3. Variable: OpVariable %ptrArrayType %var Workgroup
        // 4. Pointer to element: OpTypePointer %ptrElemType Workgroup %elementType (for OpAccessChain)
        val arrayTypeRef = currentCtx.nextResultId
        val ptrArrayTypeRef = currentCtx.nextResultId + 1
        val varRef = currentCtx.nextResultId + 2
        val ptrElemTypeRef = currentCtx.nextResultId + 3

        val insns = List(
          Instruction(Op.OpTypeArray, List(ResultRef(arrayTypeRef), ResultRef(elementTypeRef), ResultRef(arraySizeConstRef))),
          Instruction(Op.OpTypePointer, List(ResultRef(ptrArrayTypeRef), StorageClass.Workgroup, ResultRef(arrayTypeRef))),
          Instruction(Op.OpVariable, List(ResultRef(ptrArrayTypeRef), ResultRef(varRef), StorageClass.Workgroup)),
          Instruction(Op.OpTypePointer, List(ResultRef(ptrElemTypeRef), StorageClass.Workgroup, ResultRef(elementTypeRef))),
        )

        val block = SharedBlock(arrayTypeRef, varRef, ptrElemTypeRef)
        val newCtx = currentCtx.copy(
          nextResultId = currentCtx.nextResultId + 4,
          sharedVarRefs = currentCtx.sharedVarRefs + (sharedId -> block),
          workgroupPointerMap = currentCtx.workgroupPointerMap + (elementTypeRef -> ptrElemTypeRef),
        )
        (insnsAcc ::: insns, newCtx)

  // So far only used for printf
  private def getAllStrings(pending: List[GIO[?]], acc: Set[String]): Set[String] =
    pending match
      case Nil                       => acc
      case GIO.FlatMap(v, n) :: tail =>
        getAllStrings(v :: n :: tail, acc)
      case GIO.Repeat(_, gio, _) :: tail =>
        getAllStrings(gio :: tail, acc)
      case GIO.Printf(format, _*) :: tail =>
        getAllStrings(tail, acc + format)
      case _ :: tail => getAllStrings(tail, acc)

  def compile(bodyIo: GIO[?], bindings: List[GBinding[?]], workgroupSize: (Int, Int, Int) = (256, 1, 1)): ByteBuffer =
    val allExprs = getAllExprsFlattened(List(bodyIo), Nil, visitDetached = true)
    val typesInCode = allExprs.map(_.tag).distinct

    val sharedFromGio = getAllShared(List(bodyIo), Map.empty)
    val sharedFromExprs = getAllSharedFromExprs(allExprs, sharedFromGio)
    val sharedTypes = sharedFromExprs.values.map(_.tag).toList

    val allTypes = (typesInCode ::: bindings.map(_.tag) ::: sharedTypes).distinct
    def scalarTypes = allTypes.filter(_.tag <:< summon[Tag[Scalar]].tag)
    val (typeDefs, typedContext) = defineScalarTypes(scalarTypes, Context.initialContext)
    val allStrings = getAllStrings(List(bodyIo), Set.empty)
    val (stringDefs, ctxWithStrings) = defineStrings(allStrings.toList, typedContext)
    val (buffersWithIndices, uniformsWithIndices) = bindings.zipWithIndex
      .partition:
        case (_: GBuffer[?], _)  => true
        case (_: GUniform[?], _) => false
      .asInstanceOf[(List[(GBuffer[?], Int)], List[(GUniform[?], Int)])]
    val uniforms = uniformsWithIndices.map(_._1)
    val uniformSchemas = uniforms.map(_.schema)
    val structsInCode =
      (allExprs.collect {
        case cs: ComposeStruct[?] => cs.resultSchema
        case gf: GetField[?, ?]   => gf.resultSchema
      } ::: uniformSchemas).distinct
    val (structDefs, structCtx) = defineStructTypes(structsInCode, ctxWithStrings)
    val (structNames, structNamesCtx) = getStructNames(structsInCode, structCtx)
    val (decorations, uniformDefs, uniformContext) = initAndDecorateBuffers(buffersWithIndices, structNamesCtx)
    val (uniformStructDecorations, uniformStructInsns, uniformStructContext) = createAndInitUniformBlocks(uniformsWithIndices, uniformContext)
    val blockNames = getBlockNames(uniformContext, uniforms)
    val (inputDefs, inputContext) = createInvocationId(uniformStructContext, workgroupSize)

    val sharedSizeConsts = sharedFromExprs.values.map(s => (Int32Tag, s.size)).toList
    val (constDefs, constCtx) = defineConstants(allExprs, inputContext)
    val (sharedConstDefs, constCtxWithShared) = sharedSizeConsts.foldLeft((List.empty[Words], constCtx)):
      case ((insnsAcc, ctx), const) if ctx.constRefs.contains(const) => (insnsAcc, ctx)
      case ((insnsAcc, ctx), const) =>
        val insn = Instruction(Op.OpConstant, List(ResultRef(ctx.valueTypeMap(const._1.tag)), ResultRef(ctx.nextResultId), IntWord(const._2)))
        val newCtx = ctx.copy(constRefs = ctx.constRefs + (const -> ctx.nextResultId), nextResultId = ctx.nextResultId + 1)
        (insnsAcc :+ insn, newCtx)

    val (sharedDefs, ctxWithShared) = createSharedVariables(sharedFromExprs, constCtxWithShared)

    val (varDefs, varCtx) = defineVarNames(ctxWithShared)
    val (main, ctxAfterMain) = compileMain(bodyIo, varCtx)
    val (fnTypeDefs, fnDefs, ctxWithFnDefs) = compileFunctions(ctxAfterMain)
    val nameDecorations = getNameDecorations(ctxWithFnDefs)

    val code: List[Words] =
      SpirvProgramCompiler.headers(workgroupSize) ::: stringDefs ::: blockNames ::: nameDecorations ::: structNames ::: SpirvProgramCompiler.workgroupDecorations :::
        decorations ::: uniformStructDecorations ::: typeDefs ::: structDefs ::: fnTypeDefs ::: uniformDefs ::: uniformStructInsns ::: inputDefs :::
        constDefs ::: sharedConstDefs ::: sharedDefs ::: varDefs ::: main ::: fnDefs

    val fullCode = code.map:
      case WordVariable(name) if name == BOUND_VARIABLE => IntWord(ctxWithFnDefs.nextResultId)
      case x                                            => x
    val bytes = fullCode.flatMap(_.toWords).toArray

    BufferUtils.createByteBuffer(bytes.length).put(bytes).rewind()
