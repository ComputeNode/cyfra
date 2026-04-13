package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Vec4[T <: Scalar: Value] extends Vec[T] with Vec4SwizzleOps[T, Vec4[T]]
object Vec4:
  def apply[A <: FloatType: Value](x: Float, y: Float, z: Float, w: Float): Vec4[A] = const((x, y, z, w))
  def apply[A <: IntegerType: Value](x: Int, y: Int, z: Int, w: Int): Vec4[A] = const((x, y, z, w))
  given [T <: Scalar: Value]: Value[Vec4[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Vec4[T]]): Vec4[T] = new Vec4Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Vec4[T]] = Tag[Vec4[T]]
    def composites: List[Value[?]] = List(Value[T])
    def baseTag: Option[Tag[?]] = Some(Tag[Vec4].asInstanceOf[Tag[?]])
  final class Vec4Impl[T <: Scalar: Value](val block: ExpressionBlock[Vec4[T]]) extends Vec4[T] with ExpressionHolder[Vec4[T]]
