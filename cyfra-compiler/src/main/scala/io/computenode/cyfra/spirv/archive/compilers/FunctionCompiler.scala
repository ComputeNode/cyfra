//package io.computenode.cyfra.spirv.archive.compilers
//
//import io.computenode.cyfra.dsl.Expression
//import io.computenode.cyfra.dsl.macros.FnCall.FnIdentifier
//import io.computenode.cyfra.spirv.archive.Opcodes.*
//import ExpressionCompiler.compileBlock
//import SpirvProgramCompiler.bubbleUpVars
//import io.computenode.cyfra.spirv.archive.Context
//import izumi.reflect.macrortti.LightTypeTag
//
//private[cyfra] object FunctionCompiler:
//
//  case class SprivFunction(sourceFn: FnIdentifier, functionId: Int, body: Expression[?], inputArgs: List[Expression[?]]):
//    def returnType: LightTypeTag = body.tag.tag
//
//  def compileFunctionCall(call: Expression.FunctionCall[?], ctx: Context): (List[Instruction], Context) =
//    val (ctxWithFn, fn) = if ctx.functions.contains(call.fn) then
//      val fn = ctx.functions(call.fn)
//      (ctx, fn)
//    else
//      val fn = SprivFunction(call.fn, ctx.nextResultId, call.body.expr, call.args.map(_.tree))
//      val updatedCtx = ctx.copy(functions = ctx.functions + (call.fn -> fn), nextResultId = ctx.nextResultId + 1)
//      (updatedCtx, fn)
//
//    val instructions = List(
//      Instruction(
//        Op.OpFunctionCall,
//        List(ResultRef(ctxWithFn.valueTypeMap(call.tag.tag)), ResultRef(ctxWithFn.nextResultId), ResultRef(fn.functionId)) :::
//          call.exprDependencies.map(d => ResultRef(ctxWithFn.exprRefs(d.treeid))),
//      ),
//    )
//
//    val updatedContext =
//      ctxWithFn.copy(exprRefs = ctxWithFn.exprRefs + (call.treeid -> ctxWithFn.nextResultId), nextResultId = ctxWithFn.nextResultId + 1)
//    (instructions, updatedContext)
//
//  def defineFunctionTypes(ctx: Context, functions: List[SprivFunction]): (List[Words], Context) =
//    val typeDefs = functions.zipWithIndex.map { case (fn, offset) =>
//      val functionTypeId = ctx.nextResultId + offset
//      val functionTypeDef =
//        Instruction(
//          Op.OpTypeFunction,
//          List(ResultRef(functionTypeId), ResultRef(ctx.valueTypeMap(fn.returnType))) :::
//            fn.inputArgs.map(arg => ResultRef(ctx.valueTypeMap(arg.tag.tag))),
//        )
//      val functionSign = (fn.returnType, fn.inputArgs.map(_.tag.tag))
//      (functionSign, functionTypeDef, functionTypeId)
//    }
//
//    val functionTypeInstructions = typeDefs.map(_._2)
//    val functionTypeMap = typeDefs.map { case (sign, _, id) => sign -> id }.toMap
//
//    val updatedContext = ctx.copy(funcTypeMap = ctx.funcTypeMap ++ functionTypeMap, nextResultId = ctx.nextResultId + typeDefs.size)
//
//    (functionTypeInstructions, updatedContext)
//
//  def compileFunctions(ctx: Context): (List[Words], List[Words], Context) =
//
//    def compileFuncRec(ctx: Context, functions: List[SprivFunction]): (List[Words], List[Words], Context) =
//      val (functionTypeDefs, ctxWithFunTypes) = defineFunctionTypes(ctx, functions)
//      val (lastCtx, functionDefs) = functions.foldLeft(ctxWithFunTypes, List.empty[Words]) { case ((lastCtx, acc), fn) =>
//
//        val (fnInstructions, fnCtx) = compileFunction(fn, lastCtx)
//        (lastCtx.joinNested(fnCtx), acc ::: fnInstructions)
//      }
//      val newFunctions = lastCtx.functions.values.toSet.diff(ctx.functions.values.toSet)
//      if newFunctions.isEmpty then (functionTypeDefs, functionDefs, lastCtx)
//      else
//        val (newFunctionTypeDefs, newFunctionDefs, newCtx) = compileFuncRec(lastCtx, newFunctions.toList)
//        (functionTypeDefs ::: newFunctionTypeDefs, functionDefs ::: newFunctionDefs, newCtx)
//
//    compileFuncRec(ctx, ctx.functions.values.toList)
//
//  private def compileFunction(fn: SprivFunction, ctx: Context): (List[Words], Context) =
//    val opFunction = Instruction(
//      Op.OpFunction,
//      List(
//        ResultRef(ctx.valueTypeMap(fn.body.tag.tag)),
//        ResultRef(fn.functionId),
//        FunctionControlMask.Pure,
//        ResultRef(ctx.funcTypeMap((fn.returnType, fn.inputArgs.map(_.tag.tag)))),
//      ),
//    )
//    val paramsWithIndices = fn.inputArgs.zipWithIndex
//    val opFunctionParameters = paramsWithIndices.map { case (arg, i) =>
//      Instruction(Op.OpFunctionParameter, List(ResultRef(ctx.valueTypeMap(arg.tag.tag)), ResultRef(ctx.nextResultId + i)))
//    }
//    val labelId = ctx.nextResultId + fn.inputArgs.size
//    val ctxWithParameters = ctx.copy(
//      exprRefs = ctx.exprRefs ++ paramsWithIndices.map { case (arg, i) =>
//        arg.treeid -> (ctx.nextResultId + i)
//      },
//      nextResultId = labelId + 1,
//    )
//    val (bodyInstructions, bodyCtx) = compileBlock(fn.body, ctxWithParameters)
//    val (vars, nonVarsBody) = bubbleUpVars(bodyInstructions)
//    val functionInstructions = opFunction :: opFunctionParameters ::: List(Instruction(Op.OpLabel, List(ResultRef(labelId)))) ::: vars :::
//      nonVarsBody ::: List(Instruction(Op.OpReturnValue, List(ResultRef(bodyCtx.exprRefs(fn.body.treeid)))), Instruction(Op.OpFunctionEnd, List()))
//    (functionInstructions, bodyCtx)
