package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.{FunctionIR, IRs}
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.Compiler.Config
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.compiler.unit.Compilation
import io.computenode.cyfra.core.memory.{BufferRef, GBuffer, GUniform, UniformRef, Variable}
import io.computenode.cyfra.core.expression.{BuildInFunction, CustomFunction, Expression, ExpressionBlock, Value, given}

import scala.collection.mutable

class Transformer extends CompilationModule[(ExpressionBlock[Unit], Config), Compilation]:
  def compile(body: (ExpressionBlock[Unit], Config)): Compilation =
    val main = new CustomFunction("main", List(), body._1)
    val functions = extractCustomFunctions(main).reverse
    val functionMap = mutable.Map.empty[CustomFunction[?], FunctionIR[?]]
    val nextFunctions = functions.map: f =>
      val func = convertToFunction(f, functionMap)(using f.v)
      functionMap(f) = func._1
      func
    Compilation(nextFunctions, body._2)

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

  private def convertToFunction[A: Value](
    f: CustomFunction[A],
    functionMap: collection.Map[CustomFunction[?], FunctionIR[?]],
  ): (FunctionIR[A], IRs[A]) =
    val body = ExpressionBlock.optimise(f.body)
    (FunctionIR(f.name, f.arg), convertToIRs(body, functionMap, mutable.Map.empty))

  private def convertToIRs[A: Value](
    block: ExpressionBlock[A],
    functionMap: collection.Map[CustomFunction[?], FunctionIR[?]],
    expressionMap: mutable.Map[Int, IR[?]],
  ): IRs[A] =
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
      case Expression.LiteralArgs(value) =>
        IR.ConstantArgs(value)
      case x: Expression.VariableDeclare[a] =>
        given Value[a] = x.v2
        val init = x.init.map(x => convertToRefIR(x, functionMap, expressionMap))
        IR.Declare(x.variable, init)
      case Expression.Read(focus, accessChain) =>
        val chain = accessChain.map(x => convertToRefIR(x, functionMap, expressionMap))
        IR.Read(focus.getRoot, chain)
      case x: Expression.Write[a] =>
        given Value[a] = x.v2
        val chain = x.accessChain.map(x => convertToRefIR(x, functionMap, expressionMap))
        IR.Write(x.focus.getRoot, chain, convertToRefIR(x.value, functionMap, expressionMap))
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
      case x: Expression.Extract[a, A] =>
        given Value[a] = x.v2
        IR.CompositeExtract[a, A](convertToRefIR(x.value, functionMap, expressionMap), List(x.index))
      case x: Expression.Insert[A, a] =>
        given Value[a] = x.v2
        val original = convertToRefIR(x.original, functionMap, expressionMap)
        val replacement = convertToRefIR(x.replacement, functionMap, expressionMap)
        IR.CompositeInsert[A, a](original, replacement, List(x.index))
      case x: Expression.Combine[A] => simplifyCombine(x, functionMap, expressionMap)

    expressionMap(expr.id) = res
    res

  private def convertToRefIR[A](
    expr: Expression[A],
    functionMap: collection.Map[CustomFunction[?], FunctionIR[?]],
    expressionMap: mutable.Map[Int, IR[?]],
  ): IR.RefIR[A] =
    convertToIR(expr, functionMap, expressionMap) match
      case ref: IR.RefIR[A] => ref
      case _                => throw new CompilationException(s"Expected a convertable to RefIR but got: $expr")

  private def simplifyCombine[A: Value](
    combine: Expression.Combine[A],
    functionMap: collection.Map[CustomFunction[?], FunctionIR[?]],
    expressionMap: mutable.Map[Int, IR[?]],
  ): RefIR[A] =
    IR.CompositeCombine(combine.composites.map(convertToRefIR(_, functionMap, expressionMap))) // TODO replace with more sophisticated algorithm
