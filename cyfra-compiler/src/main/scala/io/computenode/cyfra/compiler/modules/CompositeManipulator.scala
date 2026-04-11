package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.Spirv.{IntWord, Op}
import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.Ctx

class CompositeManipulator extends FunctionCompilationModule:
  override def compileFunction(input: IRs[?])(using Ctx): IRs[?] =
    input.flatMapReplace:
      case x @ IR.CompositeExtract(value, ac) =>
        IRs(IR.SvRef(Op.OpCompositeExtract, Ctx.getType(x.v), value :: ac.map(IntWord.apply))(using x.v))(using x.v)
      case x @ IR.CompositeInsert(original, replacement, ac) =>
        IRs(IR.SvRef(Op.OpCompositeInsert, Ctx.getType(x.v), replacement :: original :: ac.map(IntWord.apply))(using x.v))(using x.v)
      case x @ IR.CompositeCombine(values) =>
        IRs(IR.SvRef(Op.OpCompositeConstruct, Ctx.getType(x.v), values)(using x.v))(using x.v)
      case other => IRs(other)(using other.v)
