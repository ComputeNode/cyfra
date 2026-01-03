package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.Spriv.*
import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.unit.ConstantsManager.*
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.utility.Utility.accumulate
import io.computenode.cyfra.core.expression.given
import izumi.reflect.{Tag, TagK}
import izumi.reflect.macrortti.LightTypeTag

case class ConstantsManager(block: List[IR[?]] = Nil, cache: Map[CacheKey, RefIR[?]] = Map.empty):
  def get(types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    val next = ConstantsManager.withConstant(this, types, const, value)
    val key = CacheKey(const, value.tag)
    (next.cache(key), next)

  def withIr(key: CacheKey, ir: RefIR[?]): ConstantsManager =
    if cache.contains(key) then this
    else copy(block = ir :: block, cache = cache.updated(key, ir))

  def output: List[IR[?]] = block.reverse

object ConstantsManager:
  case class CacheKey(const: Any, tag: Tag[?])

  def withConstant(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): ConstantsManager =
    val key = CacheKey(const, value.tag)
    if manager.cache.contains(key) then return manager

    value.baseTag match
      case None                       => getScalar(manager, types, const, value)._2
      case Some(t) if t <:< TagK[Vec] => getVector(manager, types, const, value)._2
      case Some(t) if t <:< TagK[Mat] => getMatrix(manager, types, const, value)._2
      case other                      => throw CompilationException(s"Cannot create constant of type: ${value.tag}")

  def getMatrix(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    val key = CacheKey(const, value.tag)
    if manager.cache.contains(key) then return (manager.cache(key), manager)

    val va = value.composite.get
    val seq = const.asInstanceOf[Product].productIterator.grouped(columns(value.baseTag.get)).toSeq

    val (scalars, m1) = seq.accumulate(manager): (acc, v) =>
      ConstantsManager.getVector(acc, types, v, va).swap

    val tpe = types.getType(value)._1
    val ir = IR.SvRef(Op.OpConstantComposite, tpe, scalars.toList)(using value)

    (ir, m1.withIr(key, ir))

  def getVector(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    val key = CacheKey(const, value.tag)
    if manager.cache.contains(key) then return (manager.cache(key), manager)

    val va = value.composite.get
    val seq = const.asInstanceOf[Product].productIterator.toSeq

    val (scalars, m1) = seq.accumulate(manager): (acc, v) =>
      ConstantsManager.getScalar(acc, types, v, va).swap

    val tpe = types.getType(value)._1
    val ir = IR.SvRef(Op.OpConstantComposite, tpe, scalars.toList)(using value)

    (ir, m1.withIr(key, ir))

  def getScalar(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    val key = CacheKey(const, value.tag)
    if manager.cache.contains(key) then return (manager.cache(key), manager)

    val tpe = types.getType(value)._1

    val ir = value.tag match
      case x if x =:= Tag[Unit] => throw CompilationException("Cannot create constant of type Unit")
      case x if x =:= Tag[Bool] =>
        val cond = const.asInstanceOf[Boolean]
        IR.SvRef[Bool](if cond then Op.OpConstantTrue else Op.OpConstantFalse, tpe, Nil)
      case x if x <:< Tag[FloatType] =>
        IR.SvRef(Op.OpConstant, tpe, List(floatToIntWord(const.asInstanceOf[Float])))(using value)
      case x if x <:< Tag[IntegerType] => IR.SvRef(Op.OpConstant, tpe, List(IntWord(const.asInstanceOf[Int])))(using value)

    (ir, manager.withIr(key, ir))

  def floatToIntWord(f: Float): IntWord =
    val bits = java.lang.Float.floatToRawIntBits(f)
    IntWord(bits)
