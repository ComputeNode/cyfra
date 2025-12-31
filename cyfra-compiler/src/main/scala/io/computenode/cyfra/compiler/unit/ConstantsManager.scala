package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.ir.IR.RefIR
import io.computenode.cyfra.compiler.spirv.Opcodes.*
import io.computenode.cyfra.compiler.CompilationException
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.utility.Utility.accumulate
import io.computenode.cyfra.core.expression.given
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import scala.collection.mutable

case class ConstantsManager(block: List[IR[?]] = Nil, cache: Map[(Any, Tag[?]), RefIR[?]] = Map.empty):
  def get(types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    val next = ConstantsManager.withConstant(this, types, const, value)
    (next.cache((const, value.tag)), next)

  def output: List[IR[?]] = block.reverse

object ConstantsManager:
  def withConstant(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): ConstantsManager =
    if manager.cache.contains((const, value.tag)) then return manager

    val t = value.tag.tag.withoutArgs
    val tArgs = value.tag.tag.typeArgs

    if tArgs.isEmpty then getScalar(manager, types, const, value)._2
    else if t <:< Tag[Vec].tag.withoutArgs then getVector(manager, types, const, value)._2
    else if t <:< Tag[Mat].tag.withoutArgs then getMatrix(manager, types, const, value)._2
    else throw CompilationException(s"Cannot create constant of type: ${value.tag}")

  def getMatrix(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    manager.cache.get((const, value.tag)) match
      case Some(ir) => return (ir, manager)
      case None     => ()

    val va = value.composite.get
    val seq = const.asInstanceOf[Seq[Any]].grouped(columns(value.tag.tag.withoutArgs)).toSeq

    val (scalars, m1) = seq.accumulate(manager): (acc, v) =>
      ConstantsManager.getVector(acc, types, v, va).swap

    val tpe = types.cache(value.tag.tag)
    val ir = IR.SvRef(Op.OpConstantComposite, tpe :: scalars.toList)(using value)

    val nextManger = m1.copy(block = ir :: m1.block, cache = m1.cache.updated((const, value.tag), ir))
    (ir, nextManger)

  def getVector(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    manager.cache.get((const, value.tag)) match
      case Some(ir) => return (ir, manager)
      case None     => ()

    val va = value.composite.get
    val seq = const.asInstanceOf[Seq[Any]]

    val (scalars, m1) = seq.accumulate(manager): (acc, v) =>
      ConstantsManager.getScalar(acc, types, v, va).swap

    val tpe = types.cache(value.tag.tag)
    val ir = IR.SvRef(Op.OpConstantComposite, tpe :: scalars.toList)(using value)

    val nextManger = m1.copy(block = ir :: m1.block, cache = m1.cache.updated((const, value.tag), ir))
    (ir, nextManger)

  def getScalar(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    manager.cache.get((const, value.tag)) match
      case Some(ir) => return (ir, manager)
      case None     => ()

    val tpe = types.cache(value.tag.tag)

    val ir = value.tag match
      case x if x =:= Tag[Unit] => throw CompilationException("Cannot create constant of type Unit")
      case x if x =:= Tag[Bool] =>
        val cond = const.asInstanceOf[Boolean]
        IR.SvRef[Bool](if cond then Op.OpConstantTrue else Op.OpConstantFalse, tpe :: Nil)
      case x if x <:< Tag[FloatType] =>
        IR.SvRef(Op.OpConstant, tpe :: floatToIntWord(const.asInstanceOf[Float]) :: Nil)(using value)
      case x if x <:< Tag[IntegerType] => IR.SvRef(Op.OpConstant, tpe :: IntWord(const.asInstanceOf[Int]) :: Nil)(using value)

    val nextManger = manager.copy(block = ir :: manager.block, cache = manager.cache.updated((const, value.tag), ir))
    (ir, nextManger)

  def floatToIntWord(f: Float): IntWord =
    val bits = java.lang.Float.floatToRawIntBits(f)
    IntWord(bits)
