package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.{Vec2Ops, VecOps}
import izumi.reflect.Tag

import scala.annotation.targetName

abstract class Vec2[T <: Scalar: Value] extends Vec[T] with Vec2Ops[T, Vec2] with VecOps[T, Vec2]:
  @targetName("vec2TimesMat2x2")
  def *(mat: Mat2x2[T])(using T <:< FloatType, Value[Vec2[T]], Value[Mat2x2[T]]): Vec2[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec2[T], Mat2x2[T], Vec2[T]](this, mat)
  @targetName("vec2TimesMat2x3")
  def *(mat: Mat2x3[T])(using T <:< FloatType, Value[Vec2[T]], Value[Mat2x3[T]], Value[Vec3[T]]): Vec3[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec2[T], Mat2x3[T], Vec3[T]](this, mat)
  @targetName("vec2TimesMat2x4")
  def *(mat: Mat2x4[T])(using T <:< FloatType, Value[Vec2[T]], Value[Mat2x4[T]], Value[Vec4[T]]): Vec4[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec2[T], Mat2x4[T], Vec4[T]](this, mat)
  @targetName("outerProductVec2")
  infix def outer(v2: Vec2[T])(using T <:< FloatType, Value[Vec2[T]], Value[Mat2x2[T]]): Mat2x2[T] =
    Value.map(Operator.OuterProduct)[Vec2[T], Vec2[T], Mat2x2[T]](this, v2)

object Vec2:
  final class Vec2Impl[T <: Scalar: Value](val block: ExpressionBlock[Vec2[T]]) extends Vec2[T] with ExpressionHolder[Vec2[T]]

  given [T <: Scalar: Value]: Value[Vec2[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Vec2[T]]): Vec2[T] = new Vec2Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Vec2[T]] = Tag[Vec2[T]]
    def composites: List[Value[?]] = List(Value[T])
    def baseTag: Option[Tag[?]] = Some(Tag[Vec2].asInstanceOf[Tag[?]])

  def apply[A <: FloatType: Value](x: Float, y: Float): Vec2[A] = const((x, y))
  def apply[A <: IntegerType: Value](x: Int, y: Int): Vec2[A] = const((x, y))
