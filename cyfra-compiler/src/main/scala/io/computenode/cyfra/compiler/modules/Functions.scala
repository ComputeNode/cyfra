package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.{Compilation, Context, Ctx}
import io.computenode.cyfra.compiler.spirv.Opcodes.Op
import io.computenode.cyfra.compiler.spirv.Opcodes.FunctionControlMask
import io.computenode.cyfra.core.expression.{Value, given}
import io.computenode.cyfra.utility.FlatList
import izumi.reflect.Tag

import scala.collection.mutable
import scala.collection

class Functions extends StandardCompilationModule:
  override def compile(input: Compilation): Compilation =
    val (newFunctions, context) = Ctx.withCapability(input.context):
      val mapRes = mutable.Buffer.empty[IRs[?]]
      input.functionBodies
        .zip(input.functions)
        .foldLeft(Map.empty[String, RefIR[Unit]]): (acc, f) =>
          val (body, pointer) = compileFunction(f._1, f._2, acc)
          mapRes.append(body)
          acc.updated(f._2.name, pointer)
      mapRes.toList
    input.copy(context = context, functionBodies = newFunctions)

  private def compileFunction(input: IRs[?], func: FunctionIR[?], funcMap: Map[String, RefIR[Unit]])(using Ctx): (IRs[?], RefIR[Unit]) =
    val definition =
      IR.SvRef[Unit](Op.OpFunction, List(Ctx.getType(input.result.v), FunctionControlMask.MaskNone, Ctx.getTypeFunction(func.v, func.parameters.headOption.map(_.v))))
    var functionArgs: List[RefIR[Unit]] = Nil
    val IRs(result, body) = input.flatMapReplace:
      case IR.SvRef(Op.OpVariable, args) if functionArgs.size < func.parameters.size =>
        val arg = IR.SvRef[Unit](Op.OpFunctionParameter, List(args.head))
        functionArgs = functionArgs :+ arg
        IRs.proxy(arg)
      case x: IR.CallWithIR[a] =>
        given Value[a] = x.v
        val IR.CallWithIR(f, args) = x
        val inst = IR.SvRef[a](Op.OpFunctionCall, List(Ctx.getType(x.v), funcMap(f.name)) ++ args)
        IRs(inst)
      case other => IRs(other)(using other.v)

    val returnInst =
      if func.v.tag =:= Tag[Unit] then IR.SvInst(Op.OpReturn, Nil)
      else IR.SvInst(Op.OpReturnValue, List(result.asInstanceOf[RefIR[?]]))
    val endInst = IR.SvInst(Op.OpFunctionEnd, Nil)

    (IRs(result, FlatList(definition, functionArgs, body, returnInst, endInst))(using result.v), definition)
