package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.Spirv.*
import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.compiler.unit.ConstantsManager.*
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import izumi.reflect.Tag

case class ConstantsManager(block: List[IR[?]] = Nil, cache: Map[CacheKey, RefIR[?]] = Map.empty):
  def output: List[IR[?]] = block.reverse

object ConstantsManager:
  case class CacheKey(const: Any, tag: Tag[?])

  def get(const: Any, value: Value[?])(using Ctx): RefIR[?] =
    val key = CacheKey(const, value.tag)
    Ctx
      .inspect(_.constants.cache.get(key))
      .getOrElse:
        val ref = value.baseTag match
          case None                        => getScalar(const, value)
          case Some(t) if t <:< Tag[Vec]   => getVector(const, value)
          case Some(t) if t <:< Tag[Mat]   => getMatrix(const, value)
          case Some(t) if t <:< Tag[Tuple] => getTuple(const, value)
          case _                           => throw CompilationException(s"Cannot create constant of type: ${value.tag}")
        Ctx.modify: context =>
          val manager = context.constants
          context.copy(constants = manager.copy(block = ref :: manager.block, cache = manager.cache.updated(key, ref)))
        ref

  private def getTuple(const: Any, value: Value[?])(using Ctx): RefIR[?] =
    val constants = const.asInstanceOf[Product].productIterator.toList
    val members = value.composites.zip(constants).map((v, c) => get(c, v))
    val tpe = TypeManager.getType(value)
    IR.SvRef(Op.OpConstantComposite, tpe, members)(using value)

  private def getMatrix(const: Any, value: Value[?])(using Ctx): RefIR[?] =
    val element = value.composites.head
    val members = const
      .asInstanceOf[Product]
      .productIterator
      .grouped(columns(value.baseTag.get))
      .map: group =>
        get(Tuple.fromArray(group.toArray), element)
    val vectors = members.toList
    val tpe = TypeManager.getType(value)
    IR.SvRef(Op.OpConstantComposite, tpe, vectors)(using value)

  private def getVector(const: Any, value: Value[?])(using Ctx): RefIR[?] =
    val element = value.composites.head
    val members = const.asInstanceOf[Product].productIterator.map(get(_, element)).toList
    val tpe = TypeManager.getType(value)
    IR.SvRef(Op.OpConstantComposite, tpe, members)(using value)

  private def getScalar(const: Any, value: Value[?])(using Ctx): RefIR[?] =
    val tpe = TypeManager.getType(value)
    value.tag match
      case x if x =:= Tag[Unit] => throw CompilationException("Cannot create constant of type Unit")
      case x if x =:= Tag[Bool] =>
        IR.SvRef[Bool](if const.asInstanceOf[Boolean] then Op.OpConstantTrue else Op.OpConstantFalse, tpe, Nil)
      case x if x =:= Tag[Float16] =>
        IR.SvRef(Op.OpConstant, tpe, List(IntWord(java.lang.Float.floatToFloat16(const.asInstanceOf[Float]).toInt & 0xffff)))(using value)
      case x if x <:< Tag[FloatType] =>
        IR.SvRef(Op.OpConstant, tpe, List(IntWord(java.lang.Float.floatToRawIntBits(const.asInstanceOf[Float]))))(using value)
      case x if x <:< Tag[IntegerType] => IR.SvRef(Op.OpConstant, tpe, List(IntWord(const.asInstanceOf[Int])))(using value)
