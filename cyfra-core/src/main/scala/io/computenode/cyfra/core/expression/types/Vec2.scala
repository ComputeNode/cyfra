package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Vec2[T <: Scalar: Value] extends Vec[T] with Vec2SwizzleOps[T, Vec2[T]]
object Vec2:
  def apply[A <: FloatType: Value](x: Float, y: Float): Vec2[A] = const((x, y))
  def apply[A <: IntegerType: Value](x: Int, y: Int): Vec2[A] = const((x, y))
  given [T <: Scalar: Value]: Value[Vec2[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Vec2[T]]): Vec2[T] = new Vec2Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Vec2[T]] = Tag[Vec2[T]]
    def composites: List[Value[?]] = List(Value[T])
    def baseTag: Option[Tag[?]] = Some(Tag[Vec2].asInstanceOf[Tag[?]])
