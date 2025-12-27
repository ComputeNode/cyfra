package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.{FunctionIR, IRs}
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.core.binding.{GBuffer, GUniform}
import io.computenode.cyfra.core.expression.{BuildInFunction, CustomFunction, Expression, ExpressionBlock, Value, Var, given}

import scala.collection.mutable

class Parser extends CompilationModule[ExpressionBlock[Unit], Compilation]:
  def compile(body: ExpressionBlock[Unit]): Compilation =
    val main = CustomFunction("main", List(), body)
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

  private def convertToFunction(f: CustomFunction[?], functionMap: mutable.Map[CustomFunction[?], FunctionIR[?]]): (FunctionIR[?], IRs[?]) = f match
    case f: CustomFunction[a] =>
      given Value[a] = f.v
      (FunctionIR(f.name, f.arg), convertToIRs(f.body, functionMap))

  private def convertToIRs[A](block: ExpressionBlock[A], functionMap: mutable.Map[CustomFunction[?], FunctionIR[?]]): IRs[A] =
    given Value[A] = block.result.v
    var result: Option[IR[A]] = None
    val body = block.body.reverse
      .distinctBy(_.id)
      .map: expr =>
        val res = convertToIR(expr, functionMap)
        if expr == block.result then result = Some(res.asInstanceOf[IR[A]])
        res
    IRs(result.get, body)

  private def convertToIR[A](expr: Expression[A], functionMap: mutable.Map[CustomFunction[?], FunctionIR[?]]): IR[A] =
    given Value[A] = expr.v
    expr match
      case Expression.Constant(value) =>
        IR.Constant[A](value)
      case x: Expression.VarDeclare[a] =>
        given Value[a] = x.v2
        IR.VarDeclare(x.variable)
      case Expression.VarRead(variable) =>
        IR.VarRead(variable)
      case x: Expression.VarWrite[a] =>
        given Value[a] = x.v2
        IR.VarWrite(x.variable, convertToIR(x.value, functionMap))
      case Expression.ReadBuffer(buffer, index) =>
        IR.ReadBuffer(buffer, convertToIR(index, functionMap))
      case x: Expression.WriteBuffer[a] =>
        given Value[a] = x.v2
        IR.WriteBuffer(x.buffer, convertToIR(x.index, functionMap), convertToIR(x.value, functionMap))
      case Expression.ReadUniform(uniform) =>
        IR.ReadUniform(uniform)
      case x: Expression.WriteUniform[a] =>
        given Value[a] = x.v2
        IR.WriteUniform(x.uniform, convertToIR(x.value, functionMap))
      case Expression.BuildInOperation(func, args) =>
        IR.Operation(func, args.map(convertToIR(_, functionMap)))
      case Expression.CustomCall(func, args) =>
        IR.Call(functionMap(func).asInstanceOf[FunctionIR[A]], args)
      case Expression.Branch(cond, ifTrue, ifFalse, break) =>
        IR.Branch(convertToIR(cond, functionMap), convertToIRs(ifTrue, functionMap), convertToIRs(ifFalse, functionMap), break)
      case Expression.Loop(mainBody, continueBody, break, continue) =>
        IR.Loop(convertToIRs(mainBody, functionMap), convertToIRs(continueBody, functionMap), break, continue)
      case x: Expression.Jump[a] =>
        given Value[a] = x.v2
        IR.Jump(x.target, convertToIR(x.value, functionMap))
      case x: Expression.ConditionalJump[a] =>
        given Value[a] = x.v2
        IR.ConditionalJump(convertToIR(x.cond, functionMap), x.target, convertToIR(x.value, functionMap))
