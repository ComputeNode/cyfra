package io.computenode.cyfra.spirv.archive.compilers

import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.Value.Scalar
import io.computenode.cyfra.dsl.binding.{GBinding, GBuffer, GUniform, WriteBuffer, WriteUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct.*
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.spirv.archive.Opcodes.*
import io.computenode.cyfra.spirv.archive.SpirvConstants.*
import io.computenode.cyfra.spirv.archive.SpirvTypes.*
import FunctionCompiler.compileFunctions
import GStructCompiler.*
import SpirvProgramCompiler.*
import io.computenode.cyfra.spirv.archive.Context
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
      case GIO.Repeat(n, gio) :: tail =>
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

  // So far only used for printf
  private def getAllStrings(pending: List[GIO[?]], acc: Set[String]): Set[String] =
    pending match
      case Nil                       => acc
      case GIO.FlatMap(v, n) :: tail =>
        getAllStrings(v :: n :: tail, acc)
      case GIO.Repeat(_, gio) :: tail =>
        getAllStrings(gio :: tail, acc)
      case GIO.Printf(format, _*) :: tail =>
        getAllStrings(tail, acc + format)
      case _ :: tail => getAllStrings(tail, acc)

  def compile(bodyIo: GIO[?], bindings: List[GBinding[?]]): ByteBuffer =
    val allExprs = getAllExprsFlattened(List(bodyIo), Nil, visitDetached = true)
    val typesInCode = allExprs.map(_.tag).distinct
    val allTypes = (typesInCode ::: bindings.map(_.tag)).distinct
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
    val (inputDefs, inputContext) = createInvocationId(uniformStructContext)
    val (constDefs, constCtx) = defineConstants(allExprs, inputContext)
    val (varDefs, varCtx) = defineVarNames(constCtx)
    val (main, ctxAfterMain) = compileMain(bodyIo, varCtx)
    val (fnTypeDefs, fnDefs, ctxWithFnDefs) = compileFunctions(ctxAfterMain)
    val nameDecorations = getNameDecorations(ctxWithFnDefs)

    val code: List[Words] =
      SpirvProgramCompiler.headers ::: stringDefs ::: blockNames ::: nameDecorations ::: structNames ::: SpirvProgramCompiler.workgroupDecorations :::
        decorations ::: uniformStructDecorations ::: typeDefs ::: structDefs ::: fnTypeDefs ::: uniformDefs ::: uniformStructInsns ::: inputDefs :::
        constDefs ::: varDefs ::: main ::: fnDefs

    val fullCode = code.map:
      case WordVariable(name) if name == BOUND_VARIABLE => IntWord(ctxWithFnDefs.nextResultId)
      case x                                            => x
    val bytes = fullCode.flatMap(_.toWords).toArray

    BufferUtils.createByteBuffer(bytes.length).put(bytes).rewind()
