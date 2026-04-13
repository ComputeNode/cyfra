package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

trait Mat[T: Value]

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

abstract class Mat2x3[T: Value] extends Mat[T]
object Mat2x3:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m02: Float, m10: Float, m11: Float, m12: Float): Mat2x3[A] = const(
    (m00, m01, m02, m10, m11, m12),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m02: Int, m10: Int, m11: Int, m12: Int): Mat2x3[A] = const((m00, m01, m02, m10, m11, m12))
  given [T <: Scalar: Value]: Value[Mat2x3[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat2x3[T]]): Mat2x3[T] = new Mat2x3Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat2x3[T]] = Tag[Mat2x3[T]]
    def composites: List[Value[?]] = List(Value[Vec3[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat2x3].asInstanceOf[Tag[?]])

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

abstract class Mat3x3[T: Value] extends Mat[T]
object Mat3x3:
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
  ): Mat3x3[A] = const((m00, m01, m02, m10, m11, m12, m20, m21, m22))
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m02: Int, m10: Int, m11: Int, m12: Int, m20: Int, m21: Int, m22: Int): Mat3x3[A] = const(
    (m00, m01, m02, m10, m11, m12, m20, m21, m22),
  )
  given [T <: Scalar: Value]: Value[Mat3x3[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat3x3[T]]): Mat3x3[T] = new Mat3x3Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat3x3[T]] = Tag[Mat3x3[T]]
    def composites: List[Value[?]] = List(Value[Vec3[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat3x3].asInstanceOf[Tag[?]])

abstract class Mat3x4[T: Value] extends Mat[T]
object Mat3x4:
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
  ): Mat3x4[A] = const((m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23))
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
  ): Mat3x4[A] = const((m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23))
  given [T <: Scalar: Value]: Value[Mat3x4[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat3x4[T]]): Mat3x4[T] = new Mat3x4Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat3x4[T]] = Tag[Mat3x4[T]]
    def composites: List[Value[?]] = List(Value[Vec4[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat3x4].asInstanceOf[Tag[?]])

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

abstract class Mat4x4[T: Value] extends Mat[T]
object Mat4x4:
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
  given [T <: Scalar: Value]: Value[Mat4x4[T]] with
    protected def extractUnsafe(ir: ExpressionBlock[Mat4x4[T]]): Mat4x4[T] = new Mat4x4Impl[T](ir)
    given Tag[T] = Value[T].tag
    def tag: Tag[Mat4x4[T]] = Tag[Mat4x4[T]]
    def composites: List[Value[?]] = List(Value[Vec4[T]])
    def baseTag: Option[Tag[?]] = Some(Tag[Mat4x4].asInstanceOf[Tag[?]])
