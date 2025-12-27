package io.computenode.cyfra.compiler.unit

import io.computenode.cyfra.compiler.ir.{IR, IRs}
import io.computenode.cyfra.compiler.ir.IR.*
import io.computenode.cyfra.compiler.spirv.Opcodes.*
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import izumi.reflect.Tag
import izumi.reflect.TagK
import izumi.reflect.macrortti.LightTypeTag

import scala.collection.mutable

case class TypeManager(block: List[IR[?]] = Nil, cache: Map[LightTypeTag, RefIR[Unit]] = Map.empty):
  def getType(value: Value[?]): (RefIR[Unit], TypeManager) = getTypeInternal(value.tag.tag)

  private def getTypeInternal(tag: LightTypeTag): (RefIR[Unit], TypeManager) =
    cache.get(tag) match
      case Some(value) => (value, this)
      case None        => TypeManager.withType(this, tag).getTypeInternal(tag)

  def output: List[IR[?]] = block.reverse

object TypeManager:
  private def withType(manager: TypeManager, tag: LightTypeTag): TypeManager =
    val t = tag.withoutArgs
    val taOpt = tag.typeArgs.headOption
    if taOpt.isEmpty then
      val ir = t match
        case BoolTag    => SvRef[Unit](Op.OpTypeBool, Nil)
        case Float16Tag => SvRef[Unit](Op.OpTypeFloat, List(IntWord(16)))
        case Float32Tag => SvRef[Unit](Op.OpTypeFloat, List(IntWord(32)))
        case Int16Tag   => SvRef[Unit](Op.OpTypeInt, List(IntWord(16), IntWord(1)))
        case Int32Tag   => SvRef[Unit](Op.OpTypeInt, List(IntWord(32), IntWord(1)))
        case UInt16Tag  => SvRef[Unit](Op.OpTypeInt, List(IntWord(16), IntWord(0)))
        case UInt32Tag  => SvRef[Unit](Op.OpTypeInt, List(IntWord(32), IntWord(0)))
      manager.copy(block = ir :: manager.block, cache = manager.cache.updated(tag, ir))
    else
      val ta = taOpt.get
      val vec2 = Vec2Tag.combine(ta)
      val vec3 = Vec3Tag.combine(ta)
      val vec4 = Vec4Tag.combine(ta)

      val (taIR, nextManager) = manager.getTypeInternal(ta)
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
