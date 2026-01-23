package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*

sealed trait Scalar

abstract class Bool extends Scalar

sealed trait NumericalType extends Scalar
sealed trait NegativeType extends NumericalType

sealed trait FloatType extends NegativeType
abstract class Float16 extends FloatType
abstract class Float32 extends FloatType

sealed trait IntegerType extends NumericalType

sealed trait SignedIntType extends IntegerType with NegativeType
abstract class Int16 extends SignedIntType
abstract class Int32 extends SignedIntType

sealed trait UnsignedIntType extends IntegerType
abstract class UInt16 extends UnsignedIntType
abstract class UInt32 extends UnsignedIntType

sealed trait Vec[T: Value]
abstract class Vec2[T: Value] extends Vec[T]
abstract class Vec3[T: Value] extends Vec[T]
abstract class Vec4[T: Value] extends Vec[T]

sealed trait Mat[T: Value]
abstract class Mat2x2[T: Value] extends Mat[T]
abstract class Mat2x3[T: Value] extends Mat[T]
abstract class Mat2x4[T: Value] extends Mat[T]
abstract class Mat3x2[T: Value] extends Mat[T]
abstract class Mat3x3[T: Value] extends Mat[T]
abstract class Mat3x4[T: Value] extends Mat[T]
abstract class Mat4x2[T: Value] extends Mat[T]
abstract class Mat4x3[T: Value] extends Mat[T]
abstract class Mat4x4[T: Value] extends Mat[T]

abstract class RuntimeArray[T: Value]

private def const[A: Value](value: Any): A =
  summon[Value[A]].extract(ExpressionBlock(Expression.Constant[A](value)))

object Float16:
  def apply(value: Float): Float16 = const(value)

object Float32:
  def apply(value: Float): Float32 = const(value)

object Int16:
  def apply(value: Int): Int16 = const(value)

object Int32:
  def apply(value: Int): Int32 = const(value)

object UInt16:
  def apply(value: Int): UInt16 = const(value)

object UInt32:
  def apply(value: Int): UInt32 = const(value)

object Bool:
  def apply(value: Boolean): Bool = const(value)

object Vec2:
  def apply[A <: FloatType: Value](x: Float, y: Float): Vec2[A] = const((x, y))
  def apply[A <: IntegerType: Value](x: Int, y: Int): Vec2[A] = const((x, y))

object Vec3:
  def apply[A <: FloatType: Value](x: Float, y: Float, z: Float): Vec3[A] = const((x, y, z))
  def apply[A <: IntegerType: Value](x: Int, y: Int, z: Int): Vec3[A] = const((x, y, z))

object Vec4:
  def apply[A <: FloatType: Value](x: Float, y: Float, z: Float, w: Float): Vec4[A] = const((x, y, z, w))
  def apply[A <: IntegerType: Value](x: Int, y: Int, z: Int, w: Int): Vec4[A] = const((x, y, z, w))

object Mat2x2:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m10: Float, m11: Float): Mat2x2[A] = const((m00, m01, m10, m11))
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m10: Int, m11: Int): Mat2x2[A] = const((m00, m01, m10, m11))

object Mat2x3:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m02: Float, m10: Float, m11: Float, m12: Float): Mat2x3[A] = const(
    (m00, m01, m02, m10, m11, m12),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m02: Int, m10: Int, m11: Int, m12: Int): Mat2x3[A] = const((m00, m01, m02, m10, m11, m12))

object Mat2x4:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m02: Float, m03: Float, m10: Float, m11: Float, m12: Float, m13: Float): Mat2x4[A] = const(
    (m00, m01, m02, m03, m10, m11, m12, m13),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m02: Int, m03: Int, m10: Int, m11: Int, m12: Int, m13: Int): Mat2x4[A] = const(
    (m00, m01, m02, m03, m10, m11, m12, m13),
  )

object Mat3x2:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m10: Float, m11: Float, m20: Float, m21: Float): Mat3x2[A] = const(
    (m00, m01, m10, m11, m20, m21),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m10: Int, m11: Int, m20: Int, m21: Int): Mat3x2[A] = const((m00, m01, m10, m11, m20, m21))

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

object Mat4x2:
  def apply[A <: FloatType: Value](m00: Float, m01: Float, m10: Float, m11: Float, m20: Float, m21: Float, m30: Float, m31: Float): Mat4x2[A] = const(
    (m00, m01, m10, m11, m20, m21, m30, m31),
  )
  def apply[A <: IntegerType: Value](m00: Int, m01: Int, m10: Int, m11: Int, m20: Int, m21: Int, m30: Int, m31: Int): Mat4x2[A] = const(
    (m00, m01, m10, m11, m20, m21, m30, m31),
  )

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
