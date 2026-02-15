package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.core.expression.{Value, given}
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.{FunctionCompilationModule, StandardCompilationModule}
import io.computenode.cyfra.compiler.unit.{Compilation, Context, Ctx}
import io.computenode.cyfra.compiler.Spirv.{Code, Decoration, IntWord, Op, StorageClass}
import io.computenode.cyfra.core.memory.{
  BindingRef,
  BufferRef,
  FocusRoot,
  GBinding,
  GBuffer,
  GUniform,
  GlobalVariable,
  LocalVariable,
  UniformRef,
  Variable,
}
import io.computenode.cyfra.utility.FlatList

import scala.collection.mutable

class Variables extends StandardCompilationModule:
  def compile(input: Compilation): Compilation =
    val (c1, globalMap) = compileGlobal(input.context)
    val (newFunctions, c2) = Ctx.withCapability(c1):
      input.functionBodies.map(compileFunction(_, globalMap))
    input.copy(context = c2, functionBodies = newFunctions)

  private def compileGlobal(input: Context): (Context, Map[FocusRoot[?], RefIR[Unit]]) =
    val ((suffix, decorations, declarations), c1) = Ctx.withCapability(input):
      val globalDeclarations = mutable.Map.empty[FocusRoot[?], RefIR[Unit]]
      val decorations = mutable.Buffer.empty[IR.SvInst]
      val hasBlockDecoration = mutable.Set.empty[IR.RefIR[?]]
      val res = input.suffix.map:
        case IR.Declare(root, None) =>
          root match
            case binding: BindingRef[?] =>
              val baseType = Ctx.getType(binding.v, decorate = true)
              val storageClass = binding match
                case _: GBuffer[?]  => StorageClass.StorageBuffer
                case _: GUniform[?] => StorageClass.Uniform
              val pointer = Ctx.getTypePointer(binding.v, storageClass)
              val variable = IR.SvRef[Unit](Op.OpVariable, pointer, List(storageClass))
              val maybeDec =
                if hasBlockDecoration(baseType) then None
                else
                  hasBlockDecoration.add(baseType)
                  Some(IR.SvInst(Op.OpDecorate, List(baseType, Decoration.Block)))
              val dec = FlatList(
                IR.SvInst(Op.OpDecorate, List(variable, Decoration.Binding, IntWord(binding.layoutOffset))),
                IR.SvInst(Op.OpDecorate, List(variable, Decoration.DescriptorSet, IntWord(0))),
                maybeDec,
              )
              decorations.appendAll(dec)
              globalDeclarations(root) = variable
              variable
            case variable: GlobalVariable[?] =>
              val baseType = Ctx.getType(variable.v)
              val storageClass = rootStorageClass(variable)
              val pointer = Ctx.getTypePointer(variable.v, storageClass)
              IR.SvRef[Unit](Op.OpVariable, pointer, List(storageClass))
            case other => ??? // how did it get here?
        case IR.Declare(root, Some(_)) => ??? // impossible, can't have starting values for global variables
        case other                     => other
      (res, decorations.toList, globalDeclarations.toMap)
    val c2 = c1.copy(suffix = suffix, decorations = c1.decorations ++ decorations)
    (c2, declarations)

  private def rootStorageClass(root: FocusRoot[?]): Code =
    root match
      case v: GlobalVariable[?] =>
        v.sharing match
          case "private"         => StorageClass.Private
          case "workgroup"       => StorageClass.Workgroup
          case "cross-workgroup" => StorageClass.CrossWorkgroup
      case _: LocalVariable[?] => StorageClass.Function
      case _: GUniform[?]      => StorageClass.Uniform
      case _: GBuffer[?]       => StorageClass.StorageBuffer

  private def compileFunction(input: IRs[?], globalVariables: Map[FocusRoot[?], RefIR[Unit]])(using Ctx): IRs[?] =
    val varDeclarations = mutable.Map.from[FocusRoot[?], RefIR[Unit]](globalVariables)
    input.flatMapReplace:
      case IR.Declare(variable, maybeInit) =>
        val inst = IR.SvRef[Unit](Op.OpVariable, Ctx.getTypePointer(variable.v, StorageClass.Function), StorageClass.Function :: maybeInit.toList)
        varDeclarations(variable) = inst
        IRs(inst)
      case IR.Write(variable, Nil, value) =>
        val inst = IR.SvInst(Op.OpStore, List(varDeclarations(variable), value))
        IRs(inst)
      case IR.Write(root, accessChain, value) =>
        val sc = rootStorageClass(root)
        val pointer = Ctx.getTypePointer(value.v, sc)
        val ac = IR.SvRef[Unit](Op.OpAccessChain, pointer, varDeclarations(root) :: accessChain ::: value :: Nil)
        val inst = IR.SvInst(Op.OpStore, List(ac, value))
        IRs(inst, List(ac, inst))
      case x: IR.Read[a] if x.accessChain.isEmpty =>
        given Value[a] = x.v
        val IR.Read(root, _) = x
        val inst = IR.SvRef[a](Op.OpLoad, Ctx.getType(x.v), List(varDeclarations(root)))
        IRs(inst)
      case x: IR.Read[a] =>
        given Value[a] = x.v
        val IR.Read(root, accessChain) = x
        val sc = rootStorageClass(root)
        val pointer = Ctx.getTypePointer(x.v, sc)
        val ac = IR.SvRef[Unit](Op.OpAccessChain, pointer, varDeclarations(root) :: accessChain)
        val inst = IR.SvRef[a](Op.OpLoad, Ctx.getType(x.v), List(varDeclarations(root)))
        IRs(inst, List(ac, inst))
      case x: IR.CallWithVar[a] =>
        given v: Value[a] = x.v
        val IR.CallWithVar(func, args) = x
        val inst = IR.CallWithIR(func, args.map(varDeclarations))
        IRs(inst)
      case other => IRs(other)(using other.v)
