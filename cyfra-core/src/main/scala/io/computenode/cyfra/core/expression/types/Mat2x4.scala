package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.MatOps
import izumi.reflect.Tag
import scala.annotation.targetName

abstract class Mat2x4[T <: Scalar: Value] extends Mat[T] with MatOps[T, Mat2x4]:
  @targetName("mat2x4TimesVec4")
  def *(vec: Vec4[T])(using T <:< FloatType, Value[Mat2x4[T]], Value[Vec4[T]], Value[Vec2[T]]): Vec2[T] =
    Value.map(Operator.MatrixTimesVector)[Mat2x4[T], Vec4[T], Vec2[T]](this, vec)
  @targetName("mat2x4TimesMat4x2")
  def *(right: Mat4x2[T])(using T <:< FloatType, Value[Mat2x4[T]], Value[Mat4x2[T]], Value[Mat2x2[T]]): Mat2x2[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x4[T], Mat4x2[T], Mat2x2[T]](this, right)
  @targetName("mat2x4TimesMat4x3")
  def *(right: Mat4x3[T])(using T <:< FloatType, Value[Mat2x4[T]], Value[Mat4x3[T]], Value[Mat2x3[T]]): Mat2x3[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x4[T], Mat4x3[T], Mat2x3[T]](this, right)
  @targetName("mat2x4TimesMat4x4")
  def *(right: Mat4x4[T])(using T <:< FloatType, Value[Mat2x4[T]], Value[Mat4x4[T]]): Mat2x4[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x4[T], Mat4x4[T], Mat2x4[T]](this, right)

object Mat2x4:
  final class Mat2x4Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat2x4[T]]) extends Mat2x4[T] with ExpressionHolder[Mat2x4[T]]

  given [T <: Scalar: Value]: Value[Mat2x4[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat2x4[T]]): Mat2x4[T] = new Mat2x4Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat2x4[T]] = Tag[Mat2x4[T]]
    def composites: List[Value[?]] = List(Value[Vec4[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat2x4].asInstanceOf[Tag[?]])

  def apply[A <: FloatType: Value](m00: Float, m01: Float, m02: Float, m03: Float, m10: Float, m11: Float, m12: Float, m13: Float): Mat2x4[A] = const(
    (m00, m01, m02, m03, m10, m11, m12, m13),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m02: Int, m03: Int, m10: Int, m11: Int, m12: Int, m13: Int): Mat2x4[A] = const(
    (m00, m01, m02, m03, m10, m11, m12, m13),
  )
