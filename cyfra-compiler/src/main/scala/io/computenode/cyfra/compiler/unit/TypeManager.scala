package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.ir.IR.*
import io.computenode.cyfra.compiler.spirv.Opcodes.*
import io.computenode.cyfra.compiler.unit.TypeManager.{FunctionTag, PointerTag}
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.compiler.unit.TypeManager.*
import izumi.reflect.{Tag, TagK, TagKK}
import izumi.reflect.macrortti.LightTypeTag

import scala.collection.mutable

case class TypeManager(block: List[IR[?]] = Nil, cache: Map[LightTypeTag, RefIR[Unit]] = Map.empty):
  def getType(tag: LightTypeTag): (RefIR[Unit], TypeManager) = getTypeInternal(tag)

  def getTypeFunction(returnType: Value[?], parameter: Option[Value[?]]): (RefIR[Unit], TypeManager) =
    val tag = FunctionTag.combine(parameter.getOrElse(Value[Unit]).tag.tag, returnType.tag.tag)
    getTypeInternal(tag)

  def getPointer(baseType: Value[?], storageClass: Code): (RefIR[Unit], TypeManager) =
    val tag = PointerTag.combine(baseType.tag.tag, intToTag(storageClass.opcode))
    val next = TypeManager.withTypePointer(this, baseType.tag.tag, storageClass)
    (next.cache(tag), next)

  def getPointer(ltag: LightTypeTag, storageClass: Code): (RefIR[Unit], TypeManager) =
    val tag = PointerTag.combine(ltag, intToTag(storageClass.opcode))
    val next = TypeManager.withTypePointer(this, ltag, storageClass)
    (next.cache(tag), next)

  private def getTypeInternal(tag: LightTypeTag): (RefIR[Unit], TypeManager) =
    val next = TypeManager.withType(this, tag)
    (next.cache(tag), next)

  def output: List[IR[?]] = block.reverse

object TypeManager:
  private trait Function[In, Out]
  val FunctionTag: LightTypeTag = TagKK[Function].tag

  private trait Pointer[Base, SC]
  val PointerTag: LightTypeTag = TagKK[Pointer].tag

  private def intToTag(v: Int): LightTypeTag = v match
    case 1  => Tag[1].tag
    case 2  => Tag[2].tag
    case 3  => Tag[3].tag
    case 4  => Tag[4].tag
    case 5  => Tag[5].tag
    case 6  => Tag[6].tag
    case 7  => Tag[7].tag
    case 8  => Tag[8].tag
    case 9  => Tag[9].tag
    case 10 => Tag[10].tag
    case 11 => Tag[11].tag
    case 12 => Tag[12].tag

  private def withType(manager: TypeManager, tag: LightTypeTag): TypeManager =
    if manager.cache.contains(tag) then return manager

    val t = tag.withoutArgs
    val tArgs = tag.typeArgs

    if tArgs.isEmpty then
      val ir = t match
        case UnitTag    => SvRef[Unit](Op.OpTypeVoid, Nil)
        case BoolTag    => SvRef[Unit](Op.OpTypeBool, Nil)
        case Float16Tag => SvRef[Unit](Op.OpTypeFloat, List(IntWord(16)))
        case Float32Tag => SvRef[Unit](Op.OpTypeFloat, List(IntWord(32)))
        case Int16Tag   => SvRef[Unit](Op.OpTypeInt, List(IntWord(16), IntWord(1)))
        case Int32Tag   => SvRef[Unit](Op.OpTypeInt, List(IntWord(32), IntWord(1)))
        case UInt16Tag  => SvRef[Unit](Op.OpTypeInt, List(IntWord(16), IntWord(0)))
        case UInt32Tag  => SvRef[Unit](Op.OpTypeInt, List(IntWord(32), IntWord(0)))
      return manager.copy(block = ir :: manager.block, cache = manager.cache.updated(tag, ir))

    val (irArgs, nextManager) = tArgs.foldRight((List.empty[RefIR[Unit]], manager)): (argTag, acc) =>
      val (irs, mgr) = acc
      val (ir, nextMgr) = mgr.getTypeInternal(argTag)
      (ir :: irs, nextMgr)

    if t =:= FunctionTag.withoutArgs then
      val argList = if tArgs(0) =:= UnitTag then List(irArgs(0)) else List(irArgs(1), irArgs(0))
      val funcIR = SvRef[Unit](Op.OpTypeFunction, argList)
      return nextManager.copy(block = funcIR :: nextManager.block, cache = nextManager.cache.updated(tag, funcIR))

    val ta = tArgs.head
    val taIR = irArgs.head

    val vec2 = Vec2Tag.combine(ta)
    val vec3 = Vec3Tag.combine(ta)
    val vec4 = Vec4Tag.combine(ta)

    val (nnManager, cIR) = t match
      case Vec2Tag                           => (nextManager, SvRef[Unit](Op.OpTypeVector, List(taIR, IntWord(2))))
      case Vec3Tag                           => (nextManager, SvRef[Unit](Op.OpTypeVector, List(taIR, IntWord(3))))
      case Vec4Tag                           => (nextManager, SvRef[Unit](Op.OpTypeVector, List(taIR, IntWord(4))))
      case Mat2x2Tag | Mat2x3Tag | Mat2x4Tag =>
        val (vIR, nnManager) = nextManager.getTypeInternal(vec2)
        (nnManager, SvRef[Unit](Op.OpTypeMatrix, List(vIR, IntWord(columns(t)))))
      case Mat3x2Tag | Mat3x3Tag | Mat3x4Tag =>
        val (vIR, nnManager) = nextManager.getTypeInternal(vec3)
        (nnManager, SvRef[Unit](Op.OpTypeMatrix, List(vIR, IntWord(columns(t)))))
      case Mat4x2Tag | Mat4x3Tag | Mat4x4Tag =>
        val (vIR, nnManager) = nextManager.getTypeInternal(vec4)
        (nnManager, SvRef[Unit](Op.OpTypeMatrix, List(vIR, IntWord(columns(t)))))
      case _ => throw new Exception(s"Unsupported type: $tag")
    nnManager.copy(block = cIR :: nnManager.block, cache = nnManager.cache.updated(tag, cIR))

  private def withTypePointer(manager: TypeManager, baseType: LightTypeTag, storageClass: Code): TypeManager =
    val tag = PointerTag.combine(baseType, intToTag(storageClass.opcode))
    if manager.cache.contains(tag) then return manager

    val (baseIR, nextManager) = manager.getTypeInternal(baseType)
    val ptrIR = SvRef[Unit](Op.OpTypePointer, List(storageClass, baseIR))
    nextManager.copy(block = ptrIR :: nextManager.block, cache = nextManager.cache.updated(tag, ptrIR))
