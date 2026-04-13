package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

trait Vec[T <: Scalar: Value] extends VecOps[T]

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
