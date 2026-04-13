package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.{Expression, ExpressionBlock, Value}
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag

private[types] def const[A: Value](value: Any): A =
  Value[A].extract(ExpressionBlock(Expression.Constant[A](value)))

def typeStride(value: Value[?]): Int =
  if value.baseTag.exists(_ <:< Tag[Tuple]) then return value.composites.map(typeStride).sum

  val elementSize = value.bottomComposite.tag match
    case t if t =:= Tag[Bool]    => throw new IllegalArgumentException("Bool has no size")
    case t if t =:= Tag[Float16] => 2
    case t if t =:= Tag[Float32] => 4
    case t if t =:= Tag[Int16]   => 2
    case t if t =:= Tag[Int32]   => 4
    case t if t =:= Tag[UInt16]  => 2
    case t if t =:= Tag[UInt32]  => 4
    case t                       => throw new NotImplementedError(s"Unknown type $t")

  val numberOfElements = value.baseTag match
    case None                         => 1
    case Some(t) if t =:= Tag[Vec2]   => 2
    case Some(t) if t =:= Tag[Vec3]   => 3
    case Some(t) if t =:= Tag[Vec4]   => 4
    case Some(t) if t =:= Tag[Mat2x2] => 4
    case Some(t) if t =:= Tag[Mat2x3] => 6
    case Some(t) if t =:= Tag[Mat2x4] => 8
    case Some(t) if t =:= Tag[Mat3x2] => 6
    case Some(t) if t =:= Tag[Mat3x3] => 9
    case Some(t) if t =:= Tag[Mat3x4] => 12
    case Some(t) if t =:= Tag[Mat4x2] => 8
    case Some(t) if t =:= Tag[Mat4x3] => 12
    case Some(t) if t =:= Tag[Mat4x4] => 16
    case Some(t) if t =:= Tag[GArray] => throw new IllegalArgumentException("GArray has infinite size")
    case t                            => throw new NotImplementedError(s"Unknown type $t")

  numberOfElements * elementSize

def rows(tag: Tag[?]): Int =
  tag match
    case t if t =:= Tag[Vec2]   => 2
    case t if t =:= Tag[Vec3]   => 3
    case t if t =:= Tag[Vec4]   => 4
    case t if t =:= Tag[Mat2x2] => 2
    case t if t =:= Tag[Mat2x3] => 2
    case t if t =:= Tag[Mat2x4] => 2
    case t if t =:= Tag[Mat3x2] => 3
    case t if t =:= Tag[Mat3x3] => 3
    case t if t =:= Tag[Mat3x4] => 3
    case t if t =:= Tag[Mat4x2] => 4
    case t if t =:= Tag[Mat4x3] => 4
    case t if t =:= Tag[Mat4x4] => 4
    case t                      => throw new NotImplementedError(s"Unknown type $t")

def columns(tag: Tag[?]): Int =
  tag match
    case t if t =:= Tag[Vec2]   => 1
    case t if t =:= Tag[Vec3]   => 1
    case t if t =:= Tag[Vec4]   => 1
    case t if t =:= Tag[Mat2x2] => 2
    case t if t =:= Tag[Mat2x3] => 3
    case t if t =:= Tag[Mat2x4] => 4
    case t if t =:= Tag[Mat3x2] => 2
    case t if t =:= Tag[Mat3x3] => 3
    case t if t =:= Tag[Mat3x4] => 4
    case t if t =:= Tag[Mat4x2] => 2
    case t if t =:= Tag[Mat4x3] => 3
    case t if t =:= Tag[Mat4x4] => 4
    case t                      => throw new NotImplementedError(s"Unknown type $t")
