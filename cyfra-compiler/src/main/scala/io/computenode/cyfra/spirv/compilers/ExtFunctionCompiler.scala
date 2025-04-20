package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.dsl.Expression.E
import io.computenode.cyfra.spirv.Opcodes.*
import io.computenode.cyfra.dsl.{Expression, FunctionName, Functions}
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.SpirvConstants.GLSL_EXT_REF


private[cyfra] object ExtFunctionCompiler:
  private val fnOpMap: Map[FunctionName, Code] = Map(
    Functions.Sin -> GlslOp.Sin,
    Functions.Cos -> GlslOp.Cos,
    Functions.Tan -> GlslOp.Tan,
    Functions.Len2 -> GlslOp.Length,
    Functions.Len3 -> GlslOp.Length,
    Functions.Pow -> GlslOp.Pow,
    Functions.Smoothstep -> GlslOp.SmoothStep,
    Functions.Sqrt -> GlslOp.Sqrt,
    Functions.Cross -> GlslOp.Cross,
    Functions.Clamp -> GlslOp.FClamp,
    Functions.Mix -> GlslOp.FMix,
    Functions.Abs -> GlslOp.FAbs,
    Functions.Atan -> GlslOp.Atan,
    Functions.Acos -> GlslOp.Acos,
    Functions.Asin -> GlslOp.Asin,
    Functions.Atan2 -> GlslOp.Atan2,
    Functions.Reflect -> GlslOp.Reflect,
    Functions.Exp -> GlslOp.Exp,
    Functions.Max -> GlslOp.FMax,
    Functions.Min -> GlslOp.FMin,
    Functions.Refract -> GlslOp.Refract,
    Functions.Normalize -> GlslOp.Normalize,
    Functions.Log -> GlslOp.Log,
  )

  def compileExtFunctionCall(call: Expression.ExtFunctionCall[_], ctx: Context): (List[Instruction], Context) =
    val fnOp = fnOpMap(call.fn)
    val tp = call.tag
    val typeRef = ctx.valueTypeMap(tp.tag)
    val instructions = List(
      Instruction(Op.OpExtInst, List(
        ResultRef(typeRef),
        ResultRef(ctx.nextResultId),
        ResultRef(GLSL_EXT_REF),
        fnOp
      ) ::: call.exprDependencies.map(d => ResultRef(ctx.exprRefs(d.treeid)))
      ))
    val updatedContext = ctx.copy(
      exprRefs = ctx.exprRefs + (call.treeid -> ctx.nextResultId),
      nextResultId = ctx.nextResultId + 1
    )
    (instructions, updatedContext)