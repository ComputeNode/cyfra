package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag
import scala.annotation.targetName

abstract class Mat3x4[T <: Scalar: Value] extends Mat[T]:
  @targetName("mat3x4TimesVec4")
  def *(vec: Vec4[T])(using T <:< FloatType, Value[Mat3x4[T]], Value[Vec4[T]], Value[Vec3[T]]): Vec3[T] =
    Value.map(Operator.MatrixTimesVector)[Mat3x4[T], Vec4[T], Vec3[T]](this, vec)
  @targetName("mat3x4TimesMat4x2")
  def *(right: Mat4x2[T])(using T <:< FloatType, Value[Mat3x4[T]], Value[Mat4x2[T]], Value[Mat3x2[T]]): Mat3x2[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat3x4[T], Mat4x2[T], Mat3x2[T]](this, right)
  @targetName("mat3x4TimesMat4x3")
  def *(right: Mat4x3[T])(using T <:< FloatType, Value[Mat3x4[T]], Value[Mat4x3[T]], Value[Mat3x3[T]]): Mat3x3[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat3x4[T], Mat4x3[T], Mat3x3[T]](this, right)
  @targetName("mat3x4TimesMat4x4")
  def *(right: Mat4x4[T])(using T <:< FloatType, Value[Mat3x4[T]], Value[Mat4x4[T]]): Mat3x4[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat3x4[T], Mat4x4[T], Mat3x4[T]](this, right)

object Mat3x4:
  final class Mat3x4Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat3x4[T]]) extends Mat3x4[T] with ExpressionHolder[Mat3x4[T]]

  given [T <: Scalar: Value]: Value[Mat3x4[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat3x4[T]]): Mat3x4[T] = new Mat3x4Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat3x4[T]] = Tag[Mat3x4[T]]
    def composites: List[Value[?]] = List(Value[Vec4[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat3x4].asInstanceOf[Tag[?]])

  def apply[A <: FloatType: Value](
    m00: Float,
    m01: Float,
    m02: Float,
    m03: Float,
    m10: Float,
    m11: Float,
    m12: Float,
    m13: Float,
    m20: Float,
    m21: Float,
    m22: Float,
    m23: Float,
  ): Mat3x4[A] = const((m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23))
  def apply[A <: IntegerType: Value](
    m00: Int,
    m01: Int,
    m02: Int,
    m03: Int,
    m10: Int,
    m11: Int,
    m12: Int,
    m13: Int,
    m20: Int,
    m21: Int,
    m22: Int,
    m23: Int,
  ): Mat3x4[A] = const((m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23))
