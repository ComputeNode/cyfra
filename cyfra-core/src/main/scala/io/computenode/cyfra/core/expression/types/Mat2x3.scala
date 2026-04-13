package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag
import scala.annotation.targetName

abstract class Mat2x3[T <: Scalar: Value] extends Mat[T]:
  @targetName("mat2x3TimesVec3")
  def *(vec: Vec3[T])(using T <:< FloatType, Value[Mat2x3[T]], Value[Vec3[T]], Value[Vec2[T]]): Vec2[T] =
    Value.map(Operator.MatrixTimesVector)[Mat2x3[T], Vec3[T], Vec2[T]](this, vec)
  @targetName("mat2x3TimesMat3x2")
  def *(right: Mat3x2[T])(using T <:< FloatType, Value[Mat2x3[T]], Value[Mat3x2[T]], Value[Mat2x2[T]]): Mat2x2[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x3[T], Mat3x2[T], Mat2x2[T]](this, right)
  @targetName("mat2x3TimesMat3x3")
  def *(right: Mat3x3[T])(using T <:< FloatType, Value[Mat2x3[T]], Value[Mat3x3[T]]): Mat2x3[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x3[T], Mat3x3[T], Mat2x3[T]](this, right)
  @targetName("mat2x3TimesMat3x4")
  def *(right: Mat3x4[T])(using T <:< FloatType, Value[Mat2x3[T]], Value[Mat3x4[T]], Value[Mat2x4[T]]): Mat2x4[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x3[T], Mat3x4[T], Mat2x4[T]](this, right)

object Mat2x3:
  final class Mat2x3Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat2x3[T]]) extends Mat2x3[T] with ExpressionHolder[Mat2x3[T]]

  given [T <: Scalar: Value]: Value[Mat2x3[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat2x3[T]]): Mat2x3[T] = new Mat2x3Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat2x3[T]] = Tag[Mat2x3[T]]
    def composites: List[Value[?]] = List(Value[Vec3[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat2x3].asInstanceOf[Tag[?]])

  def apply[A <: FloatType: Value](m00: Float, m01: Float, m02: Float, m10: Float, m11: Float, m12: Float): Mat2x3[A] = const(
    (m00, m01, m02, m10, m11, m12),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m02: Int, m10: Int, m11: Int, m12: Int): Mat2x3[A] = const((m00, m01, m02, m10, m11, m12))
