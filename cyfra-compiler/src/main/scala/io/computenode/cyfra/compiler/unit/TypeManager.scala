package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.ir.IR.*
import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.compiler.unit.TypeManager.*
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.utility.Utility.accumulate
import izumi.reflect.{Tag, TagK, TagKK}
import izumi.reflect.macrortti.LightTypeTag

import scala.collection.mutable

case class TypeManager(block: List[IR[?]] = Nil, cache: Map[CacheKey, RefIR[Unit]] = Map.empty):
  def getType(value: Value[?]): (RefIR[Unit], TypeManager) =
    val next = TypeManager.withType(this, value)
    val key = Type(value.tag)
    (next.cache(key), next)

  def getTypeFunction(returnType: Value[?], parameter: Option[Value[?]]): (RefIR[Unit], TypeManager) =
    val args = parameter.toList
    val next = TypeManager.withTypeFunction(this, returnType, args)
    val key = Function(returnType.tag, args.map(_.tag))
    (next.cache(key), next)

  def getPointer(value: Value[?], storageClass: Code): (RefIR[Unit], TypeManager) =
    val next = TypeManager.withTypePointer(this, value, storageClass)
    val key = Pointer(value.tag, storageClass.opcode)
    (next.cache(key), next)

  private def withIr(key: CacheKey, ir: RefIR[Unit]): TypeManager =
    if cache.contains(key) then this
    else copy(block = ir :: block, cache = cache.updated(key, ir))

  def output: List[IR[?]] = block.reverse

object TypeManager:
  sealed trait CacheKey extends Product
  case class Type(tag: Tag[?]) extends CacheKey
  case class Pointer(tag: Tag[?], storageClass: Int) extends CacheKey
  case class Function(result: Tag[?], args: List[Tag[?]]) extends CacheKey

  private def withType(manager: TypeManager, value: Value[?]): TypeManager =
    val key = Type(value.tag)
    if manager.cache.contains(key) then return manager

    val cOpt = value.composite

    if cOpt.isEmpty then
      val ir = value.tag match
        case t if t =:= Tag[Unit]    => SvRef[Unit](Op.OpTypeVoid, Nil)
        case t if t =:= Tag[Bool]    => SvRef[Unit](Op.OpTypeBool, Nil)
        case t if t =:= Tag[Float16] => SvRef[Unit](Op.OpTypeFloat, List(IntWord(16)))
        case t if t =:= Tag[Float32] => SvRef[Unit](Op.OpTypeFloat, List(IntWord(32)))
        case t if t =:= Tag[Int16]   => SvRef[Unit](Op.OpTypeInt, List(IntWord(16), IntWord(1)))
        case t if t =:= Tag[Int32]   => SvRef[Unit](Op.OpTypeInt, List(IntWord(32), IntWord(1)))
        case t if t =:= Tag[UInt16]  => SvRef[Unit](Op.OpTypeInt, List(IntWord(16), IntWord(0)))
        case t if t =:= Tag[UInt32]  => SvRef[Unit](Op.OpTypeInt, List(IntWord(32), IntWord(0)))
        case _                       => throw new Exception(s"Unsupported type: ${value.tag}")
      return manager.withIr(key, ir)

    val composite = cOpt.head

    val (ir, m1) = manager.getType(composite)

    val cIR = value.baseTag.get match
      case t if t <:< TagK[Vec] => SvRef[Unit](Op.OpTypeVector, List(ir, IntWord(rows(t))))
      case t if t <:< TagK[Mat] => SvRef[Unit](Op.OpTypeMatrix, List(ir, IntWord(columns(t))))
      case _                    => throw new Exception(s"Unsupported type: ${value.tag}")
    m1.withIr(key, cIR)

  private def withTypePointer(manager: TypeManager, value: Value[?], storageClass: Code): TypeManager =
    val key = Pointer(value.tag, storageClass.opcode)
    if manager.cache.contains(key) then return manager

    val (baseIR, nextManager) = manager.getType(value)
    val ptrIR = SvRef[Unit](Op.OpTypePointer, List(storageClass, baseIR))
    nextManager.copy(block = ptrIR :: nextManager.block, cache = nextManager.cache.updated(key, ptrIR))

  private def withTypeFunction(manager: TypeManager, result: Value[?], args: List[Value[?]]): TypeManager =
    val key = Function(result.tag, args.map(_.tag))
    if manager.cache.contains(key) then return manager

    val (tpe, m1) = manager.getType(result)

    val (irs, m2) = args.accumulate(m1): (mgr, v) =>
      mgr.getPointer(v, StorageClass.Function).swap

    val funcIR = SvRef[Unit](Op.OpTypeFunction, tpe :: irs.toList)
    m2.copy(block = funcIR :: m2.block, cache = m2.cache.updated(key, funcIR))
