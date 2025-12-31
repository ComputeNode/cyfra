package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.spirv.Opcodes.Code
import io.computenode.cyfra.core.expression.Value
import izumi.reflect.macrortti.LightTypeTag

case class Ctx(private var context: Context)

object Ctx:
  def withCapability[T](context: Context)(f: Ctx ?=> T): (T, Context) =
    val ctx = Ctx(context)
    val res = f(using ctx)
    (res, ctx.context)

  def getConstant[A: Value](value: Any)(using ctx: Ctx): RefIR[A] =
    val (res, next) = ctx.context.constants.get(value, Value[A].tag.tag)
    ctx.context = ctx.context.copy(constants = next)
    res.asInstanceOf[RefIR[A]]

  def getType(value: Value[?])(using ctx: Ctx): RefIR[Unit] = getType(value.tag.tag)

  def getType(tag: LightTypeTag)(using ctx: Ctx): RefIR[Unit] =
    val (res, next) = ctx.context.types.getType(tag)
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

  def getTypePointer(tag: LightTypeTag, storageClass: Code)(using ctx: Ctx): RefIR[Unit] =
    val (res, next) = ctx.context.types.getPointer(tag, storageClass)
    ctx.context = ctx.context.copy(types = next)
    res
