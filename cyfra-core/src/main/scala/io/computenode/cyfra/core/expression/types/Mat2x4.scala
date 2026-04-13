package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Mat2x4[T: Value] extends Mat[T]
object Mat2x4:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m02: Float, m03: Float, m10: Float, m11: Float, m12: Float, m13: Float): Mat2x4[A] = const(
    (m00, m01, m02, m03, m10, m11, m12, m13),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m02: Int, m03: Int, m10: Int, m11: Int, m12: Int, m13: Int): Mat2x4[A] = const(
    (m00, m01, m02, m03, m10, m11, m12, m13),
  )
  given [T <: Scalar: Value]: Value[Mat2x4[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat2x4[T]]): Mat2x4[T] = new Mat2x4Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat2x4[T]] = Tag[Mat2x4[T]]
    def composites: List[Value[?]] = List(Value[Vec4[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat2x4].asInstanceOf[Tag[?]])
