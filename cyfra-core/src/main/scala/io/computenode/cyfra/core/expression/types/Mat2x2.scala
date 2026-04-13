package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag
import scala.annotation.targetName

abstract class Mat2x2[T <: Scalar: Value] extends Mat[T]:
  @targetName("mat2x2TimesVec2")
  def *(vec: Vec2[T])(using T <:< FloatType, Value[Mat2x2[T]], Value[Vec2[T]]): Vec2[T] =
    Value.map(Operator.MatrixTimesVector)[Mat2x2[T], Vec2[T], Vec2[T]](this, vec)
  @targetName("mat2x2TimesMat2x2")
  def *(right: Mat2x2[T])(using T <:< FloatType, Value[Mat2x2[T]]): Mat2x2[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x2[T], Mat2x2[T], Mat2x2[T]](this, right)
  @targetName("mat2x2TimesMat2x3")
  def *(right: Mat2x3[T])(using T <:< FloatType, Value[Mat2x2[T]], Value[Mat2x3[T]]): Mat2x3[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x2[T], Mat2x3[T], Mat2x3[T]](this, right)
  @targetName("mat2x2TimesMat2x4")
  def *(right: Mat2x4[T])(using T <:< FloatType, Value[Mat2x2[T]], Value[Mat2x4[T]]): Mat2x4[T] =
    Value.map(Operator.MatrixTimesMatrix)[Mat2x2[T], Mat2x4[T], Mat2x4[T]](this, right)

object Mat2x2:
  final class Mat2x2Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat2x2[T]]) extends Mat2x2[T] with ExpressionHolder[Mat2x2[T]]

  given [T <: Scalar: Value]: Value[Mat2x2[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat2x2[T]]): Mat2x2[T] = new Mat2x2Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat2x2[T]] = Tag[Mat2x2[T]]
    def composites: List[Value[?]] = List(Value[Vec2[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat2x2].asInstanceOf[Tag[?]])

  def apply[A <: FloatType: Value](m00: Float, m01: Float, m10: Float, m11: Float): Mat2x2[A] = const((m00, m01, m10, m11))
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m10: Int, m11: Int): Mat2x2[A] = const((m00, m01, m10, m11))
