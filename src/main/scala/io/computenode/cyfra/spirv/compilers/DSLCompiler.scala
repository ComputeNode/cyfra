package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.dsl.{ComposeStruct, Expression, FunctionName, Functions, GArrayElem, GSeq, GStruct, GStructSchema, GetField, PhantomExpression, UniformStructRefTag, Value, WorkerIndexTag}
import izumi.reflect.Tag
import izumi.reflect.macrortti.{LTag, LTagK, LightTypeTag}
import org.lwjgl.BufferUtils
import SpirvProgramCompiler.{ArrayBufferBlock, compileMain, createAndInitUniformBlock, createInvocationId, defineConstants, defineVarNames, defineVoids, getNameDecorations, headers, initAndDecorateUniforms}
import io.computenode.cyfra.spirv.SpirvConstants.*
import io.computenode.cyfra.spirv.SpirvTypes.*
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.compileBlock
import io.computenode.cyfra.spirv.compilers.GStructCompiler.defineStructTypes
import io.computenode.cyfra.spirv.{Context}

import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.random
import scala.runtime.stdLibPatches.Predef.summon
import scala.util.Random

private[cyfra] object DSLCompiler:

  private def getAllExprsFlattened(root: E[_]): List[E[_]] = 
    var blockI = 0
    val allScopesCache = mutable.Map[Int, List[E[_]]]()
    val visited = mutable.Set[Int]()
    @tailrec
    def getAllScopesExprsAcc(toVisit: List[E[_]], acc: List[E[_]] = Nil): List[E[_]] = toVisit match
      case Nil => acc
      case e :: tail if visited.contains(e.treeid) => getAllScopesExprsAcc(tail, acc)
      case e :: tail =>
        if (allScopesCache.contains(root.treeid)) {
          return allScopesCache(root.treeid)
        }
        // Random is skipped! FromExpr???
        val eScopes = e.introducedScopes
        val newToVisit = toVisit ::: e.exprDependencies ::: eScopes.map(_.expr)
        val result = e.exprDependencies ::: acc
        visited += e.treeid
        blockI += 1
        if (blockI % 100 == 0) {
          allScopesCache.update(e.treeid, result)
        }
        getAllScopesExprsAcc(newToVisit, result)
    val result = getAllScopesExprsAcc(root :: Nil)
    allScopesCache(root.treeid) = result
    result
  
  
  def compile(tree: Value, inTypes: List[Tag[_]], outTypes: List[Tag[_]], uniformSchema: GStructSchema[_]): ByteBuffer =
    val treeExpr = tree.tree
    val allExprs = getAllExprsFlattened(treeExpr)
    val insnWithHeader = SpirvProgramCompiler.headers()
    val typesInCode = allExprs.map(_.tag).distinct
    val allTypes = (typesInCode ::: inTypes ::: outTypes).distinct
    def scalarTypes = allTypes.filter(_.tag <:< summon[Tag[Scalar]].tag)
    val (typeDefs, typedContext) = defineScalarTypes(scalarTypes, Context.initialContext)
    val structsInCode = (allExprs.collect {
      case cs: ComposeStruct[_] => cs.resultSchema
      case gf: GetField[_, _] => gf.resultSchema
    } :+ uniformSchema).distinct
    val (structDefs, structCtx) = defineStructTypes(structsInCode, typedContext)
    val (decorations, uniformDefs, uniformContext) = initAndDecorateUniforms(inTypes, outTypes, structCtx)
    val (uniformStructDecorations, uniformStructInsns, uniformStructContext) = createAndInitUniformBlock(uniformSchema, uniformContext)
    val (inputDefs, inputContext) = createInvocationId(uniformStructContext)
    val (constDefs, constCtx) = defineConstants(allExprs, inputContext)
    val (varDefs, varCtx) = defineVarNames(constCtx)
    val resultType = tree.tree.tag
    val (main, finalCtx) = compileMain(tree, resultType, varCtx)
    val nameDecorations = getNameDecorations(finalCtx)

    val code: List[Words] =
      insnWithHeader :::
        decorations :::
        nameDecorations :::
        uniformStructDecorations :::
        typeDefs :::
        structDefs :::
        uniformDefs :::
        uniformStructInsns :::
        inputDefs :::
        constDefs :::
        varDefs :::
        main

    val fullCode = code.map {
      case WordVariable(name) if name == BOUND_VARIABLE => IntWord(finalCtx.nextResultId)
      case x => x
    }
    val bytes = fullCode.flatMap(_.toWords).toArray

    BufferUtils.createByteBuffer(bytes.length).put(bytes).rewind()


