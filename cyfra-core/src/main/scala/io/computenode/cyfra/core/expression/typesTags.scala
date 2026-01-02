package io.computenode.cyfra.core.expression

import izumi.reflect.{Tag, TagK}
import izumi.reflect.macrortti.LightTypeTag

def typeStride(value: Value[?]): Int =
  val elementSize = value.bottomComposite.tag match
    case t if t =:= Tag[Bool]    => throw new IllegalArgumentException("Boolean type has no size")
    case t if t =:= Tag[Float16] => 2
    case t if t =:= Tag[Float32] => 4
    case t if t =:= Tag[Int16]   => 2
    case t if t =:= Tag[Int32]   => 4
    case t if t =:= Tag[UInt16]  => 2
    case t if t =:= Tag[UInt32]  => 4
    case _                       => ???

  val numberOfElements = value.baseTag match
    case None                         => 1
    case Some(t) if t =:= TagK[Vec2]   => 2
    case Some(t) if t =:= TagK[Vec3]   => 3
    case Some(t) if t =:= TagK[Vec4]   => 4
    case Some(t) if t =:= TagK[Mat2x2] => 4
    case Some(t) if t =:= TagK[Mat2x3] => 6
    case Some(t) if t =:= TagK[Mat2x4] => 8
    case Some(t) if t =:= TagK[Mat3x2] => 6
    case Some(t) if t =:= TagK[Mat3x3] => 9
    case Some(t) if t =:= TagK[Mat3x4] => 12
    case Some(t) if t =:= TagK[Mat4x2] => 8
    case Some(t) if t =:= TagK[Mat4x3] => 12
    case Some(t) if t =:= TagK[Mat4x4] => 16
    case _                            => ???

  numberOfElements * elementSize

def rows(tag: Tag[?]): Int =
  tag match
    case t if t =:= TagK[Vec2]   => 2
    case t if t =:= TagK[Vec3]   => 3
    case t if t =:= TagK[Vec4]   => 4
    case t if t =:= TagK[Mat2x2] => 2
    case t if t =:= TagK[Mat2x3] => 2
    case t if t =:= TagK[Mat2x4] => 2
    case t if t =:= TagK[Mat3x2] => 3
    case t if t =:= TagK[Mat3x3] => 3
    case t if t =:= TagK[Mat3x4] => 3
    case t if t =:= TagK[Mat4x2] => 4
    case t if t =:= TagK[Mat4x3] => 4
    case t if t =:= TagK[Mat4x4] => 4
    case _                       => ???

def columns(tag: Tag[?]): Int =
  tag match
    case t if t =:= TagK[Vec2]   => 1
    case t if t =:= TagK[Vec3]   => 1
    case t if t =:= TagK[Vec4]   => 1
    case t if t =:= TagK[Mat2x2] => 2
    case t if t =:= TagK[Mat2x3] => 3
    case t if t =:= TagK[Mat2x4] => 4
    case t if t =:= TagK[Mat3x2] => 2
    case t if t =:= TagK[Mat3x3] => 3
    case t if t =:= TagK[Mat3x4] => 4
    case t if t =:= TagK[Mat4x2] => 2
    case t if t =:= TagK[Mat4x3] => 3
    case t if t =:= TagK[Mat4x4] => 4
    case _                       => ???
