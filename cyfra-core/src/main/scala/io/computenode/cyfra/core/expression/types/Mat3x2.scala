package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Mat3x2[T: Value] extends Mat[T]
object Mat3x2:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m10: Float, m11: Float, m20: Float, m21: Float): Mat3x2[A] = const(
    (m00, m01, m10, m11, m20, m21),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m10: Int, m11: Int, m20: Int, m21: Int): Mat3x2[A] = const((m00, m01, m10, m11, m20, m21))
  given [T <: Scalar: Value]: Value[Mat3x2[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat3x2[T]]): Mat3x2[T] = new Mat3x2Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat3x2[T]] = Tag[Mat3x2[T]]
    def composites: List[Value[?]] = List(Value[Vec2[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat3x2].asInstanceOf[Tag[?]])
  final class Mat3x2Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat3x2[T]]) extends Mat3x2[T] with ExpressionHolder[Mat3x2[T]]
