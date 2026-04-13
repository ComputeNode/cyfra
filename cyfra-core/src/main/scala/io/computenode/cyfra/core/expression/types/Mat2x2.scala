package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Mat2x2[T: Value] extends Mat[T]
object Mat2x2:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m10: Float, m11: Float): Mat2x2[A] = const((m00, m01, m10, m11))
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m10: Int, m11: Int): Mat2x2[A] = const((m00, m01, m10, m11))
  given [T <: Scalar: Value]: Value[Mat2x2[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat2x2[T]]): Mat2x2[T] = new Mat2x2Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat2x2[T]] = Tag[Mat2x2[T]]
    def composites: List[Value[?]] = List(Value[Vec2[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat2x2].asInstanceOf[Tag[?]])
