package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Mat4x2[T: Value] extends Mat[T]
object Mat4x2:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m10: Float, m11: Float, m20: Float, m21: Float, m30: Float, m31: Float): Mat4x2[A] = const(
    (m00, m01, m10, m11, m20, m21, m30, m31),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m10: Int, m11: Int, m20: Int, m21: Int, m30: Int, m31: Int): Mat4x2[A] = const(
    (m00, m01, m10, m11, m20, m21, m30, m31),
  )
  given [T <: Scalar: Value]: Value[Mat4x2[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat4x2[T]]): Mat4x2[T] = new Mat4x2Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat4x2[T]] = Tag[Mat4x2[T]]
    def composites: List[Value[?]] = List(Value[Vec2[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat4x2].asInstanceOf[Tag[?]])
  final class Mat4x2Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat4x2[T]]) extends Mat4x2[T] with ExpressionHolder[Mat4x2[T]]
