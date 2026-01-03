package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.Spirv.Code
import io.computenode.cyfra.core.expression.Value
import izumi.reflect.macrortti.LightTypeTag

case class Ctx(private var context: Context)

object Ctx:
  def withCapability[T](context: Context)(f: Ctx ?=> T): (T, Context) =
    val ctx = Ctx(context)
    val res = f(using ctx)
    (res, ctx.context)

  def getConstant[A: Value](value: Any)(using ctx: Ctx): RefIR[A] =
    getType(Value[A])
    val (res, next) = ctx.context.constants.get(ctx.context.types, value, Value[A])
    ctx.context = ctx.context.copy(constants = next)
    res.asInstanceOf[RefIR[A]]

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
