package io.computenode.cyfra.spirv

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.spirv.Context.initialContext
import io.computenode.cyfra.spirv.Opcodes.*
import izumi.reflect.Tag
import izumi.reflect.macrortti.{LTag, LightTypeTag}

private[cyfra] object SpirvTypes:
  val Int32Tag = summon[Tag[Int32]]
  val UInt32Tag = summon[Tag[UInt32]]
  val Float32Tag = summon[Tag[Float32]]
  val GBooleanTag = summon[Tag[GBoolean]]
  val Vec2TagWithoutArgs = summon[Tag[Vec2[_]]].tag.withoutArgs
  val Vec3TagWithoutArgs = summon[Tag[Vec3[_]]].tag.withoutArgs
  val Vec4TagWithoutArgs = summon[Tag[Vec4[_]]].tag.withoutArgs
  val Vec2Tag = summon[Tag[Vec2[_]]]
  val Vec3Tag = summon[Tag[Vec3[_]]]
  val Vec4Tag = summon[Tag[Vec4[_]]]
  val VecTag = summon[Tag[Vec[_]]]
  
  val LInt32Tag = Int32Tag.tag
  val LUInt32Tag = UInt32Tag.tag
  val LFloat32Tag = Float32Tag.tag
  val LGBooleanTag = GBooleanTag.tag
  val LVec2TagWithoutArgs = Vec2TagWithoutArgs
  val LVec3TagWithoutArgs = Vec3TagWithoutArgs
  val LVec4TagWithoutArgs = Vec4TagWithoutArgs
  val LVec2Tag = Vec2Tag.tag
  val LVec3Tag = Vec3Tag.tag
  val LVec4Tag = Vec4Tag.tag
  val LVecTag = VecTag.tag

  type Vec2C[T <: Value] = Vec2[T]
  type Vec3C[T <: Value] = Vec3[T]
  type Vec4C[T <: Value] = Vec4[T]

  def scalarTypeDefInsn(tag: Tag[_], typeDefIndex: Int) = tag match {
    case Int32Tag => Instruction(Op.OpTypeInt, List(ResultRef(typeDefIndex), IntWord(32), IntWord(1)))
    case UInt32Tag => Instruction(Op.OpTypeInt, List(ResultRef(typeDefIndex), IntWord(32), IntWord(0)))
    case Float32Tag => Instruction(Op.OpTypeFloat, List(ResultRef(typeDefIndex), IntWord(32)))
    case GBooleanTag => Instruction(Op.OpTypeBool, List(ResultRef(typeDefIndex)))
  }
  
  def vecSize(tag: LightTypeTag): Int = tag match 
    case v if v <:< LVec2Tag => 2
    case v if v <:< LVec3Tag => 3
    case v if v <:< LVec4Tag => 4

  def typeStride(tag: LightTypeTag): Int = tag match
    case LInt32Tag => 4
    case LUInt32Tag => 4
    case LFloat32Tag => 4
    case LGBooleanTag => 4
    case v if v <:< LVecTag =>
      vecSize(v) * typeStride(v.typeArgs.head)
  
  def typeStride(tag: Tag[_]): Int = typeStride(tag.tag)
  
  
  def toWord(tpe: Tag[_], value: Any): Words = tpe match {
    case t if t == Int32Tag =>
      IntWord(value.asInstanceOf[Int])
    case t if t == UInt32Tag =>
      IntWord(value.asInstanceOf[Int])
    case t if t == Float32Tag =>
      val fl = value match {
        case fl: Float => fl
        case dl: Double => dl.toFloat
        case il: Int => il.toFloat
      }
      Word(intToBytes(java.lang.Float.floatToIntBits(fl)).reverse.toArray)
  }
  
  def defineScalarTypes(types: List[Tag[_]], context: Context): (List[Words], Context) = 
    val basicTypes = List(Int32Tag, Float32Tag, UInt32Tag, GBooleanTag)
    (basicTypes ::: types).distinct.foldLeft((List[Words](), context)) {
      case ((words, ctx), valType) =>
        val typeDefIndex = ctx.nextResultId
        val code = List(
          scalarTypeDefInsn(valType, typeDefIndex),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 1), StorageClass.Function, IntWord(typeDefIndex))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 2), StorageClass.Uniform, IntWord(typeDefIndex))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 3), StorageClass.Input, IntWord(typeDefIndex))),
          Instruction(Op.OpTypeVector, List(ResultRef(typeDefIndex + 4), ResultRef(typeDefIndex), IntWord(2))),
          Instruction(Op.OpTypeVector, List(ResultRef(typeDefIndex + 5), ResultRef(typeDefIndex), IntWord(3))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 6), StorageClass.Function, IntWord(typeDefIndex + 4))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 7), StorageClass.Uniform, IntWord(typeDefIndex + 4))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 8), StorageClass.Input, IntWord(typeDefIndex + 5))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 9), StorageClass.Function, IntWord(typeDefIndex + 5))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 10), StorageClass.Uniform, IntWord(typeDefIndex + 5))),
          Instruction(Op.OpTypeVector, List(ResultRef(typeDefIndex + 11), ResultRef(typeDefIndex), IntWord(4))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 12), StorageClass.Function, IntWord(typeDefIndex + 11))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 13), StorageClass.Uniform, IntWord(typeDefIndex + 11))),
          Instruction(Op.OpTypePointer, List(ResultRef(typeDefIndex + 14), StorageClass.Input, IntWord(typeDefIndex + 11))),
        )
        (code ::: words,
          ctx.copy(
            valueTypeMap = ctx.valueTypeMap ++ Map(
              valType.tag -> typeDefIndex,
              (summon[LTag[Vec2C]].tag.combine(valType.tag)) -> (typeDefIndex + 4),
              (summon[LTag[Vec3C]].tag.combine(valType.tag)) -> (typeDefIndex + 5),
              (summon[LTag[Vec4C]].tag.combine(valType.tag)) -> (typeDefIndex + 11)
            ),
            funPointerTypeMap = ctx.funPointerTypeMap ++ Map(
              typeDefIndex -> (typeDefIndex + 1),
              (typeDefIndex + 4) -> (typeDefIndex + 6),
              (typeDefIndex + 5) -> (typeDefIndex + 9),
              (typeDefIndex + 11) -> (typeDefIndex + 12)
            ),
            uniformPointerMap = ctx.uniformPointerMap ++ Map(
              typeDefIndex -> (typeDefIndex + 2),
              (typeDefIndex + 4) -> (typeDefIndex + 7),
              (typeDefIndex + 5) -> (typeDefIndex + 10),
              (typeDefIndex + 11) -> (typeDefIndex + 13)
            ),
            inputPointerMap = ctx.inputPointerMap ++ Map(
              typeDefIndex -> (typeDefIndex + 3),
              (typeDefIndex + 5) -> (typeDefIndex + 8)
            ),
            nextResultId = ctx.nextResultId + 15
          )
        )
    }
