package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.{Vec4Ops, VecOps}
import izumi.reflect.Tag

import scala.annotation.targetName

abstract class Vec4[T <: Scalar: Value] extends Vec[T] with Vec4Ops[T, Vec4] with VecOps[T, Vec4]:
  @targetName("vec4TimesMat4x2")
  def *(mat: Mat4x2[T])(using T <:< FloatType, Value[Vec4[T]], Value[Mat4x2[T]], Value[Vec2[T]]): Vec2[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec4[T], Mat4x2[T], Vec2[T]](this, mat)
  @targetName("vec4TimesMat4x3")
  def *(mat: Mat4x3[T])(using T <:< FloatType, Value[Vec4[T]], Value[Mat4x3[T]], Value[Vec3[T]]): Vec3[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec4[T], Mat4x3[T], Vec3[T]](this, mat)
  @targetName("vec4TimesMat4x4")
  def *(mat: Mat4x4[T])(using T <:< FloatType, Value[Vec4[T]], Value[Mat4x4[T]]): Vec4[T] =
    Value.map(Operator.VectorTimesMatrix)[Vec4[T], Mat4x4[T], Vec4[T]](this, mat)
  @targetName("outerProductVec4")
  infix def outer(v2: Vec4[T])(using T <:< FloatType, Value[Vec4[T]], Value[Mat4x4[T]]): Mat4x4[T] =
    Value.map(Operator.OuterProduct)[Vec4[T], Vec4[T], Mat4x4[T]](this, v2)

object Vec4:
  final class Vec4Impl[T <: Scalar: Value](val block: ExpressionBlock[Vec4[T]]) extends Vec4[T] with ExpressionHolder[Vec4[T]]

  given [T <: Scalar: Value]: Value[Vec4[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Vec4[T]]): Vec4[T] = new Vec4Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Vec4[T]] = Tag[Vec4[T]]
    def composites: List[Value[?]] = List(Value[T])
    def baseTag: Option[Tag[?]] = Some(Tag[Vec4].asInstanceOf[Tag[?]])

  def apply[A <: FloatType: Value](x: Float, y: Float, z: Float, w: Float): Vec4[A] = const((x, y, z, w))
  def apply[A <: IntegerType: Value](x: Int, y: Int, z: Int, w: Int): Vec4[A] = const((x, y, z, w))
