package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.IRs
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.modules.CompilationModule.FunctionCompilationModule
import io.computenode.cyfra.compiler.unit.Ctx
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.given
import izumi.reflect.Tag

class Constants extends FunctionCompilationModule:
  def compileFunction(input: IRs[?])(using Ctx): IRs[?] =
    input.flatMapReplace:
      case x: IR.Constant[Unit] if x.v.tag =:= Tag[Unit] =>
        IRs.proxy[Unit](x)
      case x: IR.Constant[a] =>
        given Value[a] = x.v
        IRs.proxy[a](Ctx.getConstant(x.value))
      case other => IRs(other)(using other.v)
