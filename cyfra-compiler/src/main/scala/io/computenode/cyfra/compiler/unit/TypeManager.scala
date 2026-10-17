package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.*
import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.compiler.unit.TypeManager.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.Tag

case class TypeManager(
  block: List[IR[?]] = List.empty,
  cache: Map[CacheKey, RefIR[Unit]] = Map.empty,
  decorations: List[IR[?]] = List.empty,
  decorated: Set[CacheKey] = Set.empty,
):
  def output: List[IR[?]] = decorations.reverse ++ block.reverse

object TypeManager:
  sealed trait CacheKey extends Product
  case class Type(tag: Tag[?]) extends CacheKey
  case class Pointer(tag: Tag[?], storageClass: Int) extends CacheKey
  case class Function(result: Tag[?], args: List[Tag[?]]) extends CacheKey

  def getType(value: Value[?], decorate: Boolean = false)(using Ctx): RefIR[Unit] =
    val ref = cached(Type(value.tag))(buildType(value))
    if decorate then withDecoration(value)
    ref

  def getTypeFunction(returnType: Value[?], parameter: Option[Value[?]])(using Ctx): RefIR[Unit] =
    val args = parameter.toList
    cached(Function(returnType.tag, args.map(_.tag))):
      val result = getType(returnType)
      val parameters = args.map(getPointer(_, StorageClass.Function))
      SvRef[Unit](Op.OpTypeFunction, result :: parameters)

  def getPointer(value: Value[?], storageClass: Code)(using Ctx): RefIR[Unit] =
    cached(Pointer(value.tag, storageClass.opcode)):
      val base = getType(value)
      storageClass match
        case StorageClass.StorageBuffer if containsHalf(value) => ExtensionManager.enableCapability(Capability.StorageBuffer16BitAccess)
        case StorageClass.Uniform if containsHalf(value)       => ExtensionManager.enableCapability(Capability.UniformAndStorageBuffer16BitAccess)
        case _                                                 => ()
      SvRef[Unit](Op.OpTypePointer, List(storageClass, base))

  private def cached(key: CacheKey)(build: => RefIR[Unit])(using Ctx): RefIR[Unit] =
    Ctx
      .inspect(_.types.cache.get(key))
      .getOrElse:
        val ref = build
        Ctx.modify: context =>
          val manager = context.types
          context.copy(types = manager.copy(block = ref :: manager.block, cache = manager.cache.updated(key, ref)))
        ref

  private def buildType(value: Value[?])(using Ctx): RefIR[Unit] =
    if value.composites.isEmpty then
      return value.tag match
        case t if t =:= Tag[Unit]    => SvRef[Unit](Op.OpTypeVoid, Nil)
        case t if t =:= Tag[Bool]    => SvRef[Unit](Op.OpTypeBool, Nil)
        case t if t =:= Tag[Float16] =>
          ExtensionManager.enableCapability(Capability.Float16)
          SvRef[Unit](Op.OpTypeFloat, List(IntWord(16)))
        case t if t =:= Tag[Float32] => SvRef[Unit](Op.OpTypeFloat, List(IntWord(32)))
        case t if t =:= Tag[Int16]   => SvRef[Unit](Op.OpTypeInt, List(IntWord(16), IntWord(1)))
        case t if t =:= Tag[Int32]   => SvRef[Unit](Op.OpTypeInt, List(IntWord(32), IntWord(1)))
        case t if t =:= Tag[UInt16]  => SvRef[Unit](Op.OpTypeInt, List(IntWord(16), IntWord(0)))
        case t if t =:= Tag[UInt32]  => SvRef[Unit](Op.OpTypeInt, List(IntWord(32), IntWord(0)))
        case _                       => throw new Exception(s"Unsupported type: ${value.tag}")

    val elements = value.composites.map(getType(_))
    value.arrayLength match
      case Some(size) =>
        val length = ConstantsManager.get(size, Value[UInt32])
        return SvRef[Unit](Op.OpTypeArray, List(elements.head, length))
      case _ => ()

    value.baseTag.get match
      case t if t <:< Tag[Vec]          => SvRef[Unit](Op.OpTypeVector, List(elements.head, IntWord(rows(t))))
      case t if t <:< Tag[Mat]          => SvRef[Unit](Op.OpTypeMatrix, List(elements.head, IntWord(columns(t))))
      case t if t =:= Tag[RuntimeArray] => SvRef[Unit](Op.OpTypeRuntimeArray, List(elements.head))
      case t if t <:< Tag[Tuple]        => SvRef[Unit](Op.OpTypeStruct, elements)
      case _                            => throw new Exception(s"Unsupported type: ${value.tag}")

  private def containsHalf(value: Value[?]): Boolean =
    value.tag =:= Tag[Float16] || value.composites.exists(containsHalf)

  private def withDecoration(value: Value[?])(using Ctx): Unit =
    val key = Type(value.tag)
    if Ctx.inspect(_.types.decorated(key)) then return

    val tpe = Ctx.inspect(_.types.cache(key))
    value.composites.foreach(withDecoration)
    val decorations: List[IR[?]] = value.baseTag match
      case None                                                        => Nil
      case Some(t) if t =:= Tag[RuntimeArray] || t =:= Tag[SizedArray] =>
        val List(element) = value.composites
        List(IR.SvInst(Op.OpDecorate, List(tpe, Decoration.ArrayStride, IntWord(typeStride(element)))))
      case Some(t) if t <:< Tag[Tuple] =>
        value.composites.zipWithIndex
          .foldLeft((0, List.empty[IR[?]])):
            case ((offset, instructions), (member, index)) =>
              val inst = IR.SvInst(Op.OpMemberDecorate, List(tpe, IntWord(index), Decoration.Offset, IntWord(offset)))
              val nextOffset = if member.baseTag.exists(_ =:= Tag[RuntimeArray]) then Int.MinValue else offset + typeStride(member)
              (nextOffset, inst :: instructions)
          ._2
      case Some(t) if t <:< Tag[Vec] => Nil
      case Some(t) if t <:< Tag[Mat] => List(IR.SvInst(Op.OpDecorate, List(tpe, Decoration.ColMajor)))
    Ctx.modify: context =>
      val manager = context.types
      context.copy(types = manager.copy(decorations = decorations ++ manager.decorations, decorated = manager.decorated + key))
