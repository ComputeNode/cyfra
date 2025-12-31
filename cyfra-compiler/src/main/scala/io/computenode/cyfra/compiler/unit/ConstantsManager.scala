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
  def get(value: Any, tag: LightTypeTag): (RefIR[?], ConstantsManager) =
    val next = ConstantsManager.withConstant(this, value, tag)
    (next.cache((value, tag)), next)

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
    val t = tag.withoutArgs
    val tArgs = tag.typeArgs

    ???

  def getVector(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    manager.cache.get((value, tag)) match
      case Some(ir) => return (ir, manager)
      case None     => ()

    val t = tag.withoutArgs
    val ta = tag.typeArgs.head
    val l = value.asInstanceOf[Seq[Any]]

    val (scalars, m1) = l.accumulate(manager): (acc, v) =>
      val (nIr, m) = ConstantsManager.getScalar(acc, v, ta)
      scalars.addOne(nIr)
      (m, nIr)

    val tpe = types.cache(tag)

  def getScalar(manager: ConstantsManager, types: TypeManager, const: Any, value: Value[?]): (RefIR[?], ConstantsManager) =
    manager.cache.get((const, value.tag)) match
      case Some(ir) => return (ir, manager)
      case None     => ()

    val tpe = types.cache(value.tag.tag)

    val ir = value.tag.tag match
      case UnitTag => throw CompilationException("Cannot create constant of type Unit")
      case BoolTag =>
        val cond = value.asInstanceOf[Boolean]
        IR.SvRef[Bool](if cond then Op.OpConstantTrue else Op.OpConstantFalse, tpe :: Nil)
      case Float16Tag =>
        val bits = java.lang.Float.floatToRawIntBits(value.asInstanceOf[Float])
        IR.SvRef[Float16](Op.OpConstant, tpe :: IntWord(bits) :: Nil)
      case Float32Tag =>
        val bits = java.lang.Float.floatToRawIntBits(value.asInstanceOf[Float])
        IR.SvRef[Float32](Op.OpConstant, tpe :: IntWord(bits) :: Nil)
      case Int16Tag  => IR.SvRef[Int16](Op.OpConstant, tpe :: IntWord(value.asInstanceOf[Int]) :: Nil)
      case Int32Tag  => IR.SvRef[Int32](Op.OpConstant, tpe :: IntWord(value.asInstanceOf[Int]) :: Nil)
      case UInt16Tag => IR.SvRef[UInt16](Op.OpConstant, tpe :: IntWord(value.asInstanceOf[Int]) :: Nil)
      case UInt32Tag => IR.SvRef[UInt32](Op.OpConstant, tpe :: IntWord(value.asInstanceOf[Int]) :: Nil)

    val nextManger = manager.copy(block = ir :: manager.block, cache = manager.cache.updated((value, tag), ir))
    (ir, nextManger)
