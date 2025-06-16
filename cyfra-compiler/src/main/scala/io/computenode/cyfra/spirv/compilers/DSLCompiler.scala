package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.*
import io.computenode.cyfra.spirv.Opcodes.*
import izumi.reflect.Tag
import izumi.reflect.macrortti.{LTag, LTagK, LightTypeTag}
import org.lwjgl.BufferUtils
import SpirvProgramCompiler.*
import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.Value.Scalar
import io.computenode.cyfra.dsl.struct.GStruct.*
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.spirv.SpirvConstants.*
import io.computenode.cyfra.spirv.SpirvTypes.*
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.compileBlock
import io.computenode.cyfra.spirv.compilers.GStructCompiler.*
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.compilers.FunctionCompiler.{compileFunctions, defineFunctionTypes}

import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.random
import scala.runtime.stdLibPatches.Predef.summon
import scala.util.Random

private[cyfra] object DSLCompiler:

  // TODO: Not traverse same fn scopes for each fn call
  private def getAllExprsFlattened(root: E[?], visitDetached: Boolean): List[E[?]] =
    var blockI = 0
    val allScopesCache = mutable.Map[Int, List[E[?]]]()
    val visited = mutable.Set[Int]()
    @tailrec
    def getAllScopesExprsAcc(toVisit: List[E[?]], acc: List[E[?]] = Nil): List[E[?]] = toVisit match
      case Nil                                     => acc
      case e :: tail if visited.contains(e.treeid) => getAllScopesExprsAcc(tail, acc)
      case e :: tail                               =>
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

  def compile(tree: Value, inTypes: List[Tag[?]], outTypes: List[Tag[?]], uniformSchema: GStructSchema[?]): ByteBuffer =
    val treeExpr = tree.tree
    val allExprs = getAllExprsFlattened(treeExpr, visitDetached = true)
    val typesInCode = allExprs.map(_.tag).distinct
    val allTypes = (typesInCode ::: inTypes ::: outTypes).distinct
    def scalarTypes = allTypes.filter(_.tag <:< summon[Tag[Scalar]].tag)
    val (typeDefs, typedContext) = defineScalarTypes(scalarTypes, Context.initialContext)
    val structsInCode =
      (allExprs.collect {
        case cs: ComposeStruct[?] => cs.resultSchema
        case gf: GetField[?, ?]   => gf.resultSchema
      } :+ uniformSchema).distinct
    val (structDefs, structCtx) = defineStructTypes(structsInCode, typedContext)
    val structNames = getStructNames(structsInCode, structCtx)
    val (decorations, uniformDefs, uniformContext) = initAndDecorateUniforms(inTypes, outTypes, structCtx)
    val (uniformStructDecorations, uniformStructInsns, uniformStructContext) = createAndInitUniformBlock(uniformSchema, uniformContext)
    val blockNames = getBlockNames(uniformContext, uniformSchema)
    val (inputDefs, inputContext) = createInvocationId(uniformStructContext)
    val (constDefs, constCtx) = defineConstants(allExprs, inputContext)
    val (varDefs, varCtx) = defineVarNames(constCtx)
    val resultType = tree.tree.tag
    val (main, ctxAfterMain) = compileMain(tree, resultType, varCtx)
    val (fnTypeDefs, fnDefs, ctxWithFnDefs) = compileFunctions(ctxAfterMain)
    val nameDecorations = getNameDecorations(ctxWithFnDefs)

    val code: List[Words] =
      SpirvProgramCompiler.headers ::: blockNames ::: nameDecorations ::: structNames ::: SpirvProgramCompiler.workgroupDecorations :::
        decorations ::: uniformStructDecorations ::: typeDefs ::: structDefs ::: fnTypeDefs ::: uniformDefs ::: uniformStructInsns ::: inputDefs :::
        constDefs ::: varDefs ::: main ::: fnDefs

    val fullCode = code.map {
      case WordVariable(name) if name == BOUND_VARIABLE => IntWord(ctxWithFnDefs.nextResultId)
      case x                                            => x
    }
    val bytes = fullCode.flatMap(_.toWords).toArray

    BufferUtils.createByteBuffer(bytes.length).put(bytes).rewind()
