package io.computenode.cyfra.compiler.modules

import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.ir.{FunctionIR, IR, IRs}
import io.computenode.cyfra.compiler.spirv.Opcodes.{Decoration, IntWord, Op, StorageClass}
import io.computenode.cyfra.compiler.modules.CompilationModule.{FunctionCompilationModule, StandardCompilationModule}
import io.computenode.cyfra.compiler.unit.{Compilation, Context, Ctx}
import io.computenode.cyfra.core.binding.{GBinding, GBuffer, GUniform}
import io.computenode.cyfra.core.expression.{Int32, Value, typeStride, given}
import io.computenode.cyfra.utility.FlatList
import izumi.reflect.macrortti.LightTypeTag

class Bindings extends StandardCompilationModule:
  override def compile(input: Compilation): Compilation =
    val (nextCompilation, variables) = prepareHeader(input)
    val (nFunctions, nextContext) = Ctx.withCapability(nextCompilation.context):
      nextCompilation.functionBodies.map: func =>
        compileFunction(func, variables.zipWithIndex.map(_.swap).toMap)
    nextCompilation.copy(context = nextContext, functionBodies = nFunctions)

  private def prepareHeader(input: Compilation): (Compilation, List[RefIR[Unit]]) =
    val (res, context) = Ctx.withCapability(input.context):
      val mapped = input.bindings.zipWithIndex.map: (binding, idx) =>
        val baseType = Ctx.getType(binding.v)
        val array = binding match
          case buffer: GBuffer[?]   => Some(IR.SvRef[Unit](Op.OpTypeRuntimeArray, List(baseType)))
          case uniform: GUniform[?] => None
        val struct = IR.SvRef[Unit](Op.OpTypeStruct, List(array.getOrElse(baseType)))
        val pointer = IR.SvRef[Unit](Op.OpTypePointer, List(StorageClass.StorageBuffer, struct))

        val types: List[RefIR[Unit]] = FlatList(array, struct, pointer)

        val variable: RefIR[Unit] = IR.SvRef[Unit](Op.OpVariable, pointer, List(StorageClass.StorageBuffer))

        val decorations: List[IR[?]] =
          FlatList(
            IR.SvInst(Op.OpDecorate, List(variable, Decoration.Binding, IntWord(0))),
            IR.SvInst(Op.OpDecorate, List(variable, Decoration.DescriptorSet, IntWord(idx))),
            IR.SvInst(Op.OpDecorate, List(struct, Decoration.Block)),
            IR.SvInst(Op.OpMemberDecorate, List(struct, IntWord(0), Decoration.Offset, IntWord(0))),
            array.map(i => IR.SvInst(Op.OpDecorate, List(i, Decoration.ArrayStride, IntWord(typeStride(binding.v))))),
          )

        (decorations, types, variable)
      val (decorations, types, variables) = mapped.unzip3
      (decorations.flatten, types.flatten, variables)

    val nContext = context.copy(decorations = context.decorations ++ res._1, suffix = context.suffix ++ res._2 ++ res._3)
    (input.copy(context = nContext), res._3.toList)

  private def compileFunction(input: IRs[?], variables: Map[Int, RefIR[Unit]])(using Ctx): IRs[?] =
    input.flatMapReplace:
      case x: IR.ReadUniform[a] =>
        given Value[a] = x.v
        val IR.ReadUniform(uniform) = x
        val value = Ctx.getType(uniform.v)
        val ptrValue = Ctx.getTypePointer(uniform.v, StorageClass.StorageBuffer)
        val accessChain = IR.SvRef[Unit](Op.OpAccessChain, ptrValue, List(variables(uniform.layoutOffset), Ctx.getConstant[Int32](0)))
        val loadInst = IR.SvRef[a](Op.OpLoad, value, List(accessChain))
        IRs(loadInst, List(accessChain, loadInst))
      case x: IR.ReadBuffer[a] =>
        given Value[a] = x.v
        val IR.ReadBuffer(buffer, idx) = x
        val value = Ctx.getType(buffer.v)
        val ptrValue = Ctx.getTypePointer(buffer.v, StorageClass.StorageBuffer)
        val accessChain = IR.SvRef[Unit](Op.OpAccessChain, ptrValue, List(variables(buffer.layoutOffset), Ctx.getConstant[Int32](0), idx))
        val loadInst = IR.SvRef[a](Op.OpLoad, value, List(accessChain))
        IRs(loadInst, List(accessChain, loadInst))
      case IR.WriteUniform(uniform, value) =>
        val value = Ctx.getType(uniform.v)
        val ptrValue = Ctx.getTypePointer(uniform.v, StorageClass.StorageBuffer)
        val accessChain = IR.SvRef[Unit](Op.OpAccessChain, ptrValue, List(variables(uniform.layoutOffset), Ctx.getConstant[Int32](0)))
        val storeInst = IR.SvInst(Op.OpStore, List(accessChain, value))
        IRs(storeInst, List(accessChain, storeInst))
      case IR.WriteBuffer(buffer, index, value) =>
        val valueType = Ctx.getType(buffer.v)
        val ptrValue = Ctx.getTypePointer(buffer.v, StorageClass.StorageBuffer)
        val accessChain = IR.SvRef[Unit](Op.OpAccessChain, ptrValue, List(variables(buffer.layoutOffset), Ctx.getConstant[Int32](0), index))
        val storeInst = IR.SvInst(Op.OpStore, List(accessChain, value))
        IRs(storeInst, List(accessChain, storeInst))
      case other => IRs(other)(using other.v)
