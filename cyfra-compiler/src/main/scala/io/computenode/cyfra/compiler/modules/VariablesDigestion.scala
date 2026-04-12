package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.modules.CompilationModule.StandardCompilationModule
import io.computenode.cyfra.compiler.unit.{CompilationUnit, Context, Ctx}
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.types.{UInt32, given}
import io.computenode.cyfra.core.memory.*
import io.computenode.cyfra.core.memory.GlobalVariable.Sharing.{CrossWorkgroup, Private, Workgroup}
import io.computenode.cyfra.utility.FlatList

import scala.collection.mutable

class VariablesDigestion extends StandardCompilationModule:
  def compile(input: CompilationUnit): CompilationUnit =
    val (c1, globalMap) = compileGlobal(input.context)
    val (newFunctions, c2) = Ctx.withCapability(c1):
      input.functionBodies.map(compileFunction(_, globalMap))
    input.copy(context = c2, functionBodies = newFunctions)

  private def compileGlobal(input: Context): (Context, Map[FocusRoot[?], RefIR[Unit]]) =
    val ((suffix, decorations, prefix, declarations), c1) = Ctx.withCapability(input):
      val globalDeclarations = mutable.Map.empty[FocusRoot[?], RefIR[Unit]]
      val decorations = mutable.Buffer.empty[IR.SvInst]
      val hasBlockDecoration = mutable.Set.empty[IR.RefIR[?]]
      val interface = mutable.Buffer.empty[IR.Interface]
      val res = input.suffix.map:
        case IR.Declare(root, None) =>
          root match
            case binding: BindingRef[?] =>
              val tupleValue = Value.surroundWithTuple(binding.v)
              val baseType = Ctx.getType(tupleValue, decorate = true)
              val storageClass = binding match
                case _: GBuffer[?]  => StorageClass.StorageBuffer
                case _: GUniform[?] => StorageClass.Uniform
              val pointer = Ctx.getTypePointer(tupleValue, storageClass)
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
              interface.addOne(IR.Interface(variable))
              variable
            case variable: GlobalVariable[?] =>
              val baseType = Ctx.getType(variable.v)
              val storageClass = rootStorageClass(variable)
              val pointer = Ctx.getTypePointer(variable.v, storageClass)
              val res = IR.SvRef[Unit](Op.OpVariable, pointer, List(storageClass))
              globalDeclarations(root) = res
              res
            case variable: BuildInVariable[?] =>
              val baseType = Ctx.getType(variable.v)
              val storageClass = rootStorageClass(variable)
              val pointer = Ctx.getTypePointer(variable.v, storageClass)
              val res = IR.SvRef[Unit](Op.OpVariable, pointer, List(storageClass))
              globalDeclarations(root) = res
              interface.addOne(IR.Interface(res))
              variable match
                case BuildInVariable.GlobalInvocationId =>
                  decorations.append(IR.SvInst(Op.OpDecorate, List(res, Decoration.BuiltIn, BuiltIn.GlobalInvocationId)))
              res
            case other => ??? // how did it get here?
        case IR.Declare(root, Some(_)) => ??? // impossible, can't have starting values for global variables
        case other                     => other
      (res, decorations.toList, interface.toList, globalDeclarations.toMap)
    val c2 = c1.copy(prefix = c1.prefix ++ prefix, decorations = c1.decorations ++ decorations, suffix = suffix)
    (c2, declarations)

  private def rootStorageClass(root: FocusRoot[?]): Code =
    root match
      case v: GlobalVariable[?] =>
        v.sharing match
          case Private        => StorageClass.Private
          case Workgroup      => StorageClass.Workgroup
          case CrossWorkgroup => StorageClass.CrossWorkgroup
      case _: LocalVariable[?]   => StorageClass.Function
      case _: BuildInVariable[?] => StorageClass.Input
      case _: GUniform[?]        => StorageClass.Uniform
      case _: GBuffer[?]         => StorageClass.StorageBuffer

  private def compileFunction(input: IRs[?], globalVariables: Map[FocusRoot[?], RefIR[Unit]])(using Ctx): IRs[?] =
    val varDeclarations = mutable.Map.from[FocusRoot[?], RefIR[Unit]](globalVariables)
    input.flatMapReplace:
      case IR.Declare(variable, maybeInit) =>
        val inst = IR.SvRef[Unit](Op.OpVariable, Ctx.getTypePointer(variable.v, StorageClass.Function), StorageClass.Function :: maybeInit.toList)
        varDeclarations(variable) = inst
        IRs(inst)
      case IR.Write(variable, Nil, value) if !variable.isInstanceOf[GBinding[?]] =>
        val inst = IR.SvInst(Op.OpStore, List(varDeclarations(variable), value))
        IRs(inst)
      case IR.Write(root, accessChainRaw, value) =>
        val accessChain = if root.isInstanceOf[GBinding[?]] then Ctx.getConstant[UInt32](0) :: accessChainRaw else accessChainRaw
        val sc = rootStorageClass(root)
        val pointer = Ctx.getTypePointer(value.v, sc)
        val ac = IR.SvRef[Unit](Op.OpAccessChain, pointer, varDeclarations(root) :: accessChain)
        val inst = IR.SvInst(Op.OpStore, List(ac, value))
        IRs(inst, List(ac, inst))
      case x: IR.Read[a] if x.accessChain.isEmpty && !x.root.isInstanceOf[GBinding[?]] =>
        given Value[a] = x.v
        val IR.Read(root, _) = x
        val inst = IR.SvRef[a](Op.OpLoad, Ctx.getType(x.v), List(varDeclarations(root)))
        IRs(inst)
      case x: IR.Read[a] =>
        given Value[a] = x.v
        val IR.Read(root, accessChainRaw) = x
        val accessChain = if root.isInstanceOf[GBinding[?]] then Ctx.getConstant[UInt32](0) :: accessChainRaw else accessChainRaw
        val sc = rootStorageClass(root)
        val pointer = Ctx.getTypePointer(x.v, sc)
        val ac = IR.SvRef[Unit](Op.OpAccessChain, pointer, varDeclarations(root) :: accessChain)
        val inst = IR.SvRef[a](Op.OpLoad, Ctx.getType(x.v), List(ac))
        IRs(inst, List(ac, inst))
      case x: IR.CallWithVar[a] =>
        given v: Value[a] = x.v
        val IR.CallWithVar(func, args) = x
        val inst = IR.CallWithIR(func, args.map(varDeclarations))
        IRs(inst)
      case other => IRs(other)(using other.v)
