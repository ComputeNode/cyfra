package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.compiler.unit.ExtensionManager.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

case class ExtensionManager(
  capabilities: List[IR[?]] = List.empty,
  capabilitySet: Set[Int] = Set.empty,
  extensions: List[IR[?]] = List.empty,
  extensionSet: Set[String] = Set.empty,
  imports: List[IR[?]] = List.empty,
  importCache: Map[String, RefIR[Unit]] = Map.empty,
):
  def enableCapability(capability: Code): ExtensionManager =
    ExtensionManager.withCapability(this, capability)

  def enableExtension(name: String): ExtensionManager =
    ExtensionManager.withExtension(this, name)

  def getExtInstImport(name: String): (RefIR[Unit], ExtensionManager) =
    val next = ExtensionManager.withExtInstImport(this, name)
    (next.importCache(name), next)

  def output: List[IR[?]] = capabilities.reverse ++ extensions.reverse ++ imports.reverse

object ExtensionManager:
  private def withCapability(manager: ExtensionManager, capability: Code): ExtensionManager =
    if manager.capabilitySet.contains(capability.opcode) then return manager

    val ir = IR.SvInst(Op.OpCapability, List(capability))
    manager.copy(capabilities = ir :: manager.capabilities, capabilitySet = manager.capabilitySet + capability.opcode)

  private def withExtension(manager: ExtensionManager, name: String): ExtensionManager =
    if manager.extensionSet.contains(name) then return manager

    val ir = IR.SvInst(Op.OpExtension, List(Text(name)))
    manager.copy(extensions = ir :: manager.extensions, extensionSet = manager.extensionSet + name)

  private def withExtInstImport(manager: ExtensionManager, name: String): ExtensionManager =
    if manager.importCache.contains(name) then return manager

    val ir = IR.SvRef[Unit](Op.OpExtInstImport, List(Text(name)))
    manager.copy(imports = ir :: manager.imports, importCache = manager.importCache.updated(name, ir))
