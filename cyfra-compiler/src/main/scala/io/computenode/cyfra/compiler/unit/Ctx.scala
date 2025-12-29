package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.spirv.Opcodes.Code
import io.computenode.cyfra.core.expression.Value

case class Ctx(private var context: Context)

object Ctx:
  def withCapability[T](context: Context)(f: Ctx ?=> T): (T, Context) =
    val ctx = Ctx(context)
    val res = f(using ctx)
    (res, ctx.context)

  def getType(value: Value[?])(using ctx: Ctx): RefIR[Unit] =
    val (res, next) = ctx.context.types.getType(value)
    ctx.context = ctx.context.copy(types = next)
    res

  def getTypeFunction(returnType: Value[?], parameter: Option[Value[?]])(using ctx: Ctx): RefIR[Unit] =
    val (res, next) = ctx.context.types.getTypeFunction(returnType, parameter)
    ctx.context = ctx.context.copy(types = next)
    res

  def getTypePointer(value: Value[?], storageClass: Code)(using ctx: Ctx): RefIR[Unit] =
    val (res, next) = ctx.context.types.getPointer(value, storageClass)
    ctx.context = ctx.context.copy(types = next)
    res
