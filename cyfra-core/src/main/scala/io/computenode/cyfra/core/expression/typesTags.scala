package io.computenode.cyfra.core.expression

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

val BoolTag = summon[Tag[Bool]].tag
val Float16Tag = summon[Tag[Float16]].tag
val Float32Tag = summon[Tag[Float32]].tag
val Int16Tag = summon[Tag[Int16]].tag
val Int32Tag = summon[Tag[Int32]].tag
val UInt16Tag = summon[Tag[UInt16]].tag
val UInt32Tag = summon[Tag[UInt32]].tag

val Vec2Tag = summon[Tag[Vec2[?]]].tag.withoutArgs
val Vec3Tag = summon[Tag[Vec3[?]]].tag.withoutArgs
val Vec4Tag = summon[Tag[Vec4[?]]].tag.withoutArgs

val Mat2x2Tag = summon[Tag[Mat2x2[?]]].tag.withoutArgs
val Mat2x3Tag = summon[Tag[Mat2x3[?]]].tag.withoutArgs
val Mat2x4Tag = summon[Tag[Mat2x4[?]]].tag.withoutArgs
val Mat3x2Tag = summon[Tag[Mat3x2[?]]].tag.withoutArgs
val Mat3x3Tag = summon[Tag[Mat3x3[?]]].tag.withoutArgs
val Mat3x4Tag = summon[Tag[Mat3x4[?]]].tag.withoutArgs
val Mat4x2Tag = summon[Tag[Mat4x2[?]]].tag.withoutArgs
val Mat4x3Tag = summon[Tag[Mat4x3[?]]].tag.withoutArgs
val Mat4x4Tag = summon[Tag[Mat4x4[?]]].tag.withoutArgs

def typeStride(value: Value[?]): Int = typeStride(value.tag)
def typeStride(tag: Tag[?]): Int = typeStride(tag.tag)

private def typeStride(tag: LightTypeTag): Int =
  val elementSize = tag.typeArgs.headOption.map(typeStride).getOrElse(1)
  val base = tag.withoutArgs match
    case BoolTag    => ???
    case Float16Tag => 2
    case Float32Tag => 4
    case Int16Tag   => 2
    case Int32Tag   => 4
    case UInt16Tag  => 2
    case UInt32Tag  => 4
    case Vec2Tag    => 2
    case Vec3Tag    => 3
    case Vec4Tag    => 4
    case Mat2x2Tag  => 4
    case Mat2x3Tag  => 6
    case Mat2x4Tag  => 8
    case Mat3x2Tag  => 6
    case Mat3x3Tag  => 9
    case Mat3x4Tag  => 12
    case Mat4x2Tag  => 8
    case Mat4x3Tag  => 12
    case Mat4x4Tag  => 16
    case _          => ???

  base * elementSize
