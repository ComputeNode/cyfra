package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Mat4x3[T: Value] extends Mat[T]
object Mat4x3:
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
    m30: Float,
    m31: Float,
    m32: Float,
  ): Mat4x3[A] = const((m00, m01, m02, m10, m11, m12, m20, m21, m22, m30, m31, m32))
  def apply[A <: IntegerType: Value](
    m00: Int,
    m01: Int,
    m02: Int,
    m10: Int,
    m11: Int,
    m12: Int,
    m20: Int,
    m21: Int,
    m22: Int,
    m30: Int,
    m31: Int,
    m32: Int,
  ): Mat4x3[A] = const((m00, m01, m02, m10, m11, m12, m20, m21, m22, m30, m31, m32))
  given [T <: Scalar: Value]: Value[Mat4x3[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat4x3[T]]): Mat4x3[T] = new Mat4x3Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat4x3[T]] = Tag[Mat4x3[T]]
    def composites: List[Value[?]] = List(Value[Vec3[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat4x3].asInstanceOf[Tag[?]])
