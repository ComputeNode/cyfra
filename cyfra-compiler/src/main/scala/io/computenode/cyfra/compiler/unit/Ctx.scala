package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.Spirv.Code
import io.computenode.cyfra.core.expression.Value

case class Ctx(private var context: Context)

object Ctx:
  def withCapability[T](context: Context)(f: Ctx ?=> T): (T, Context) =
    val ctx = Ctx(context)
    val res = f(using ctx)
    (res, ctx.context)

  def inspect[A](f: Context => A)(using ctx: Ctx): A = f(ctx.context)

  /** Apply a pure update to the current context, after resolving any manager dependencies. */
  def modify(f: Context => Context)(using ctx: Ctx): Unit =
    ctx.context = f(ctx.context)

  def getConstant[A: Value](value: Any)(using Ctx): RefIR[A] =
    ConstantsManager.get(value, Value[A]).asInstanceOf[RefIR[A]]

  def getType(value: Value[?], decorate: Boolean = false)(using Ctx): RefIR[Unit] =
    TypeManager.getType(value, decorate)

  def getTypeFunction(returnType: Value[?], parameter: Option[Value[?]])(using Ctx): RefIR[Unit] =
    TypeManager.getTypeFunction(returnType, parameter)

  def getTypePointer(value: Value[?], storageClass: Code)(using Ctx): RefIR[Unit] =
    TypeManager.getPointer(value, storageClass)

  def enableCapability(capability: Code)(using Ctx): Unit =
    ExtensionManager.enableCapability(capability)

  def enableExtension(name: String)(using Ctx): Unit =
    ExtensionManager.enableExtension(name)

  def getExtInstImport(name: String)(using Ctx): RefIR[Unit] =
    ExtensionManager.getExtInstImport(name)
