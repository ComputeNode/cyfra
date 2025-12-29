package io.computenode.cyfra.core.expression

import izumi.reflect.{Tag, TagK}
import izumi.reflect.macrortti.LightTypeTag

val UnitTag = Tag[Unit].tag
val BoolTag = Tag[Bool].tag

val Float16Tag = Tag[Float16].tag
val Float32Tag = Tag[Float32].tag
val Int16Tag = Tag[Int16].tag
val Int32Tag = Tag[Int32].tag
val UInt16Tag = Tag[UInt16].tag
val UInt32Tag = Tag[UInt32].tag

val Vec2Tag = TagK[Vec2].tag
val Vec3Tag = TagK[Vec3].tag
val Vec4Tag = TagK[Vec4].tag

val Mat2x2Tag = TagK[Mat2x2].tag
val Mat2x3Tag = TagK[Mat2x3].tag
val Mat2x4Tag = TagK[Mat2x4].tag
val Mat3x2Tag = TagK[Mat3x2].tag
val Mat3x3Tag = TagK[Mat3x3].tag
val Mat3x4Tag = TagK[Mat3x4].tag
val Mat4x2Tag = TagK[Mat4x2].tag
val Mat4x3Tag = TagK[Mat4x3].tag
val Mat4x4Tag = TagK[Mat4x4].tag

def typeStride(value: Value[?]): Int = typeStride(value.tag)
def typeStride(tag: Tag[?]): Int = typeStride(tag.tag)

private def typeStride(tag: LightTypeTag): Int =
  val elementSize = tag.typeArgs.headOption.map(typeStride).getOrElse(1)
  val base = tag match
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

def columns(tag: LightTypeTag): Int =
  tag match
    case Mat2x2Tag => 2
    case Mat2x3Tag => 3
    case Mat2x4Tag => 4
    case Mat3x2Tag => 2
    case Mat3x3Tag => 3
    case Mat3x4Tag => 4
    case Mat4x2Tag => 2
    case Mat4x3Tag => 3
    case Mat4x4Tag => 4
    case _         => ???
