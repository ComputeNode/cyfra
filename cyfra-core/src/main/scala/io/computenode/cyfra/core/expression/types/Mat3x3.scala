package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag
import scala.annotation.targetName

abstract class Mat3x3[T <: Scalar: Value] extends Mat[T]:
  @targetName("mat3x3TimesVec3")
  def *(vec: Vec3[T])(using T <:< FloatType, Value[Mat3x3[T]], Value[Vec3[T]]): Vec3[T] =
    Value.map(Operator.MatrixTimesVector)[Mat3x3[T], Vec3[T], Vec3[T]](this, vec)
  @targetName("mat3x3TimesMat3x2")
  def *(right: Mat3x2[T])(using T <:< FloatType, Value[Mat3x3[T]], Value[Mat3x2[T]]): Mat3x2[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat3x3[T], Mat3x2[T], Mat3x2[T]](this, right)
  @targetName("mat3x3TimesMat3x3")
  def *(right: Mat3x3[T])(using T <:< FloatType, Value[Mat3x3[T]]): Mat3x3[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat3x3[T], Mat3x3[T], Mat3x3[T]](this, right)
  @targetName("mat3x3TimesMat3x4")
  def *(right: Mat3x4[T])(using T <:< FloatType, Value[Mat3x3[T]], Value[Mat3x4[T]]): Mat3x4[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat3x3[T], Mat3x4[T], Mat3x4[T]](this, right)

object Mat3x3:
  final class Mat3x3Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat3x3[T]]) extends Mat3x3[T] with ExpressionHolder[Mat3x3[T]]

  given [T <: Scalar: Value]: Value[Mat3x3[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat3x3[T]]): Mat3x3[T] = new Mat3x3Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat3x3[T]] = Tag[Mat3x3[T]]
    def composites: List[Value[?]] = List(Value[Vec3[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat3x3].asInstanceOf[Tag[?]])

  def apply[A <: FloatType: Value](
    m00: Float,
    m01: Float,
    m02: Float,
    m10: Float,
    m11: Float,
    m12: Float,
    m20: Float,
    m21: Float,
    m22: Float,
  ): Mat3x3[A] = const((m00, m01, m02, m10, m11, m12, m20, m21, m22))
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m02: Int, m10: Int, m11: Int, m12: Int, m20: Int, m21: Int, m22: Int): Mat3x3[A] = const(
    (m00, m01, m02, m10, m11, m12, m20, m21, m22),
  )
