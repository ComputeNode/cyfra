package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.ops.MatOps
import izumi.reflect.Tag
import scala.annotation.targetName

abstract class Mat4x2[T <: Scalar: Value] extends Mat[T] with MatOps[T, Mat4x2]:
  @targetName("mat4x2TimesVec2")
  def *(vec: Vec2[T])(using T <:< FloatType, Value[Mat4x2[T]], Value[Vec2[T]], Value[Vec4[T]]): Vec4[T] =
    Value.map(Operator.MatrixTimesVector)[Mat4x2[T], Vec2[T], Vec4[T]](this, vec)
  @targetName("mat4x2TimesMat2x2")
  def *(right: Mat2x2[T])(using T <:< FloatType, Value[Mat4x2[T]], Value[Mat2x2[T]]): Mat4x2[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat4x2[T], Mat2x2[T], Mat4x2[T]](this, right)
  @targetName("mat4x2TimesMat2x3")
  def *(right: Mat2x3[T])(using T <:< FloatType, Value[Mat4x2[T]], Value[Mat2x3[T]], Value[Mat4x3[T]]): Mat4x3[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat4x2[T], Mat2x3[T], Mat4x3[T]](this, right)
  @targetName("mat4x2TimesMat2x4")
  def *(right: Mat2x4[T])(using T <:< FloatType, Value[Mat4x2[T]], Value[Mat2x4[T]], Value[Mat4x4[T]]): Mat4x4[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat4x2[T], Mat2x4[T], Mat4x4[T]](this, right)

object Mat4x2:
  final class Mat4x2Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat4x2[T]]) extends Mat4x2[T] with ExpressionHolder[Mat4x2[T]]

  given [T <: Scalar: Value]: Value[Mat4x2[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat4x2[T]]): Mat4x2[T] = new Mat4x2Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat4x2[T]] = Tag[Mat4x2[T]]
    def composites: List[Value[?]] = List(Value[Vec2[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat4x2].asInstanceOf[Tag[?]])

  def apply[A <: FloatType: Value](m00: Float, m01: Float, m10: Float, m11: Float, m20: Float, m21: Float, m30: Float, m31: Float): Mat4x2[A] = const(
    (m00, m01, m10, m11, m20, m21, m30, m31),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m10: Int, m11: Int, m20: Int, m21: Int, m30: Int, m31: Int): Mat4x2[A] = const(
    (m00, m01, m10, m11, m20, m21, m30, m31),
  )
