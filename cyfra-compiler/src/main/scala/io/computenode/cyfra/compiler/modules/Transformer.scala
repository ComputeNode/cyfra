package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.{FunctionIR, IRs}
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.core.binding.{BufferRef, GBuffer, GUniform, UniformRef}
import io.computenode.cyfra.core.expression.{BuildInFunction, CustomFunction, Expression, ExpressionBlock, Value, Var, given}

import scala.collection.mutable

class Transformer extends CompilationModule[ExpressionBlock[Unit], Compilation]:
  def compile(body: ExpressionBlock[Unit]): Compilation =
    val main = new CustomFunction("main", List(), body)
    val functions = extractCustomFunctions(main).reverse
    val functionMap = mutable.Map.empty[CustomFunction[?], FunctionIR[?]]
    val nextFunctions = functions.map: f =>
      val func = convertToFunction(f, functionMap)
      functionMap(f) = func._1
      func
    Compilation(nextFunctions)

  private def extractCustomFunctions(f: CustomFunction[Unit]): List[CustomFunction[?]] =
    val visited = mutable.Map[CustomFunction[?], 0 | 1 | 2]().withDefaultValue(0)

    def rec(f: CustomFunction[?]): List[CustomFunction[?]] =
      visited(f) match
        case 0 =>
          visited(f) = 1
          val fs = f.body
            .collect:
              case cc: Expression.CustomCall[?] => cc.func
            .flatMap(rec)
          visited(f) = 2
          f :: fs
        case 1 => throw new CompilationException(s"Cyclic dependency detected involving function: ${f.name}")
        case 2 => Nil // Already processed

    rec(f)

  private def convertToFunction(f: CustomFunction[?], functionMap: collection.Map[CustomFunction[?], FunctionIR[?]]): (FunctionIR[?], IRs[?]) =
    f match
      case f: CustomFunction[a] =>
        given Value[a] = f.v
        (FunctionIR(f.name, f.arg), convertToIRs(f.body, functionMap, mutable.Map.empty))

  private def convertToIRs[A](
    block: ExpressionBlock[A],
    functionMap: collection.Map[CustomFunction[?], FunctionIR[?]],
    expressionMap: mutable.Map[Int, IR[?]],
  ): IRs[A] =
    given Value[A] = block.result.v
    var result: Option[IR[A]] = None
    val body = block.body.reverse
      .distinctBy(_.id)
      .map: expr =>
        val res = convertToIR(expr, functionMap, expressionMap)
        if expr == block.result then result = Some(res.asInstanceOf[IR[A]])
        res
    IRs(result.get, body)

  private def convertToIR[A](
    expr: Expression[A],
    functionMap: collection.Map[CustomFunction[?], FunctionIR[?]],
    expressionMap: mutable.Map[Int, IR[?]],
  ): IR[A] =
    given Value[A] = expr.v
    if expressionMap.contains(expr.id) then return expressionMap(expr.id).asInstanceOf[IR[A]]
    val res: IR[A] = expr match
      case Expression.Constant(value) =>
        IR.Constant[A](value)
      case x: Expression.VarDeclare[a] =>
        given Value[a] = x.v2
        IR.VarDeclare(x.variable)
      case Expression.VarRead(variable) =>
        IR.VarRead(variable)
      case x: Expression.VarWrite[a] =>
        given Value[a] = x.v2
        IR.VarWrite(x.variable, convertToRefIR(x.value, functionMap, expressionMap))
      case Expression.ReadBuffer(buffer, index) =>
        IR.ReadBuffer(asBufferRef(buffer), convertToRefIR(index, functionMap, expressionMap))
      case x: Expression.WriteBuffer[a] =>
        given Value[a] = x.v2
        IR.WriteBuffer(asBufferRef(x.buffer), convertToRefIR(x.index, functionMap, expressionMap), convertToRefIR(x.value, functionMap, expressionMap))
      case Expression.ReadUniform(uniform) =>
        IR.ReadUniform(asUniformRef(uniform))
      case x: Expression.WriteUniform[a] =>
        given Value[a] = x.v2
        IR.WriteUniform(asUniformRef(x.uniform), convertToRefIR(x.value, functionMap, expressionMap))
      case Expression.BuildInOperation(func, args) =>
        IR.Operation(func, args.map(convertToRefIR(_, functionMap, expressionMap)))
      case Expression.CustomCall(func, args) =>
        IR.CallWithVar(functionMap(func).asInstanceOf[FunctionIR[A]], args)
      case Expression.Branch(cond, ifTrue, ifFalse, break) =>
        IR.Branch(
          convertToRefIR(cond, functionMap, expressionMap),
          convertToIRs(ifTrue, functionMap, expressionMap),
          convertToIRs(ifFalse, functionMap, expressionMap),
          break,
        )
      case Expression.Loop(mainBody, continueBody, break, continue) =>
        IR.Loop(convertToIRs(mainBody, functionMap, expressionMap), convertToIRs(continueBody, functionMap, expressionMap), break, continue)
      case x: Expression.Jump[a] =>
        given Value[a] = x.v2
        IR.Jump(x.target, convertToRefIR(x.value, functionMap, expressionMap))
      case x: Expression.ConditionalJump[a] =>
        given Value[a] = x.v2
        IR.ConditionalJump(convertToRefIR(x.cond, functionMap, expressionMap), x.target, convertToRefIR(x.value, functionMap, expressionMap))

    expressionMap(expr.id) = res
    res

  def asBufferRef[A](buffer: GBuffer[A]): BufferRef[A] = buffer match
    case x: BufferRef[A] => x
    case _               => throw new CompilationException(s"Expected BufferRef but got: $buffer")

  def asUniformRef[A](uniform: GUniform[A]): UniformRef[A] = uniform match
    case x: UniformRef[A] => x
    case _                => throw new CompilationException(s"Expected UniformRef but got: $uniform")

  private def convertToRefIR[A](
    expr: Expression[A],
    functionMap: collection.Map[CustomFunction[?], FunctionIR[?]],
    expressionMap: mutable.Map[Int, IR[?]],
  ): IR.RefIR[A] =
    convertToIR(expr, functionMap, expressionMap) match
      case ref: IR.RefIR[A] => ref
      case _                => throw new CompilationException(s"Expected a convertable to RefIR but got: $expr")
