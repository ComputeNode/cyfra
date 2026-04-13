package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Mat4x4[T: Value] extends Mat[T]
object Mat4x4:
  final class Mat4x4Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat4x4[T]]) extends Mat4x4[T] with ExpressionHolder[Mat4x4[T]]

  given [T <: Scalar: Value]: Value[Mat4x4[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat4x4[T]]): Mat4x4[T] = new Mat4x4Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat4x4[T]] = Tag[Mat4x4[T]]
    def composites: List[Value[?]] = List(Value[Vec4[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat4x4].asInstanceOf[Tag[?]])

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
    m30: Float,
    m31: Float,
    m32: Float,
    m33: Float,
  ): Mat4x4[A] = const((m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33))
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
    m30: Int,
    m31: Int,
    m32: Int,
    m33: Int,
  ): Mat4x4[A] = const((m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33))
