package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

abstract class Vec3[T <: Scalar: Value] extends Vec[T] with Vec3SwizzleOps[T, Vec3[T]]
object Vec3:
  def apply[A <: FloatType: Value](x: Float, y: Float, z: Float): Vec3[A] = const((x, y, z))
  def apply[A <: IntegerType: Value](x: Int, y: Int, z: Int): Vec3[A] = const((x, y, z))
  given [T <: Scalar: Value]: Value[Vec3[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Vec3[T]]): Vec3[T] = new Vec3Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Vec3[T]] = Tag[Vec3[T]]
    def composites: List[Value[?]] = List(Value[T])
    def baseTag: Option[Tag[?]] = Some(Tag[Vec3].asInstanceOf[Tag[?]])
