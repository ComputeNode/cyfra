package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.core.expression.types.given

case class ExtensionManager(
  capabilities: List[IR[?]] = List.empty,
  capabilitySet: Set[Int] = Set.empty,
  extensions: List[IR[?]] = List.empty,
  extensionSet: Set[String] = Set.empty,
  imports: List[IR[?]] = List.empty,
  importCache: Map[String, RefIR[Unit]] = Map.empty,
):
  def output: List[IR[?]] = capabilities.reverse ++ extensions.reverse ++ imports.reverse

object ExtensionManager:
  def enableCapability(capability: Code)(using Ctx): Unit =
    if Ctx.inspect(_.extensions.capabilitySet(capability.opcode)) then return
    val ir = IR.SvInst(Op.OpCapability, List(capability))
    Ctx.modify: context =>
      val manager = context.extensions
      context.copy(extensions = manager.copy(capabilities = ir :: manager.capabilities, capabilitySet = manager.capabilitySet + capability.opcode))

  def enableExtension(name: String)(using Ctx): Unit =
    if Ctx.inspect(_.extensions.extensionSet(name)) then return
    val ir = IR.SvInst(Op.OpExtension, List(Text(name)))
    Ctx.modify: context =>
      val manager = context.extensions
      context.copy(extensions = manager.copy(extensions = ir :: manager.extensions, extensionSet = manager.extensionSet + name))

  def getExtInstImport(name: String)(using Ctx): RefIR[Unit] =
    Ctx
      .inspect(_.extensions.importCache.get(name))
      .getOrElse:
        val ir = IR.SvRef[Unit](Op.OpExtInstImport, List(Text(name)))
        Ctx.modify: context =>
          val manager = context.extensions
          context.copy(extensions = manager.copy(imports = ir :: manager.imports, importCache = manager.importCache.updated(name, ir)))
        ir
