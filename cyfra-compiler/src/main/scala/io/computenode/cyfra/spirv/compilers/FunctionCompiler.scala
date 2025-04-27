package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.dsl.{Expression, FunctionName, Functions}
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.compilers.FunctionCompiler.SprivFunction
import io.computenode.cyfra.spirv.SpirvConstants.GLSL_EXT_REF
import io.computenode.cyfra.dsl.macros.Source
import io.computenode.cyfra.dsl.Control
import io.computenode.cyfra.spirv.compilers.ExpressionCompiler.compileBlock

private[cyfra] object FunctionCompiler:

  case class SprivFunction(sourceFn: Source.PureIdentifier, functionId: Int, body: Expression[_], inputArgs: List[Expression[_]])

  def compileFunctionCall(call: Expression.FunctionCall[_], ctx: Context): (List[Instruction], Context) =
    val (ctxWithFn, fn) = if ctx.functions.contains(call.fn) then
      val fn = ctx.functions(call.fn)
      (ctx, fn)
    else
      val fn = SprivFunction(call.fn, ctx.nextResultId, call.body.expr, call.args.map(_.tree))
      val updatedCtx = ctx.copy(
        functions = ctx.functions + (call.fn -> fn),
        nextResultId = ctx.nextResultId + 1
      )
      (updatedCtx, fn)

    val instructions = List(Instruction(Op.OpFunctionCall, List(
      ResultRef(ctxWithFn.valueTypeMap(call.tag.tag)),
      ResultRef(ctxWithFn.nextResultId),
      ResultRef(fn.functionId)
    ) ::: call.exprDependencies.map(d => ResultRef(ctxWithFn.exprRefs(d.treeid)))))

    val updatedContext = ctx.copy(
      exprRefs = ctxWithFn.exprRefs + (call.treeid -> ctxWithFn.nextResultId),
      nextResultId = ctxWithFn.nextResultId + 1
    )
    (instructions, updatedContext)
  
  def compileFunctions(ctx: Context): (List[Words], Context) =
    val functionDefinitions = ctx.functions.values.toList.map { fn =>
      val (fnInstructions, fnCtx) = compileFunction(fn, ctx)
      (fnInstructions, fnCtx)
    }
    val instructions = functionDefinitions.flatMap(_._1)
    val updatedContext = functionDefinitions.map(_._2).reduce(_.joinNested(_))
    (instructions, updatedContext)
    
  def compileFunction(fn: SprivFunction, ctx: Context): (List[Words], Context) =
    val opFunction = Instruction(Op.OpFunction, List(
      ResultRef(ctx.valueTypeMap(fn.body.tag.tag)),
      ResultRef(ctx.nextResultId),
      ResultRef(fn.functionId),
      StorageClass.Function
    ))
    val paramsWithIndices = fn.inputArgs.zipWithIndex
    val opFunctionParameters = paramsWithIndices.map { case (arg, i) =>
      Instruction(Op.OpFunctionParameter, List(
        ResultRef(ctx.valueTypeMap(arg.tag.tag)),
        ResultRef(ctx.nextResultId + i + 1)
      ))
    }
    val ctxWithParameters = ctx.copy(
      exprRefs = ctx.exprRefs ++ paramsWithIndices.map { case (arg, i) =>
        arg.treeid -> (ctx.nextResultId + i + 1)
      },
      nextResultId = ctx.nextResultId + fn.inputArgs.size + 1
    )
    val (bodyInstructions, bodyCtx) = compileBlock(
      fn.body,
      ctxWithParameters
    )
    val functionInstructions = List(
      opFunction,
      Instruction(Op.OpLabel, List(ResultRef(ctx.nextResultId))),
    ) ::: bodyInstructions ::: List(
      Instruction(Op.OpReturnValue, List(ResultRef(bodyCtx.exprRefs(fn.body.treeid)))),
      Instruction(Op.OpFunctionEnd, List())
    )
    (functionInstructions, bodyCtx)