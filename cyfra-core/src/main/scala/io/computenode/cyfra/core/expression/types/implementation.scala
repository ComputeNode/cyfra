package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*

final class Float16Impl(val block: ExpressionBlock[Float16]) extends Float16 with ExpressionHolder[Float16]
final class Float32Impl(val block: ExpressionBlock[Float32]) extends Float32 with ExpressionHolder[Float32]
final class Int16Impl(val block: ExpressionBlock[Int16]) extends Int16 with ExpressionHolder[Int16]
final class Int32Impl(val block: ExpressionBlock[Int32]) extends Int32 with ExpressionHolder[Int32]
final class UInt16Impl(val block: ExpressionBlock[UInt16]) extends UInt16 with ExpressionHolder[UInt16]
final class UInt32Impl(val block: ExpressionBlock[UInt32]) extends UInt32 with ExpressionHolder[UInt32]
final class BoolImpl(val block: ExpressionBlock[Bool]) extends Bool with ExpressionHolder[Bool]

final class Vec2Impl[T <: Scalar: Value](val block: ExpressionBlock[Vec2[T]]) extends Vec2[T] with ExpressionHolder[Vec2[T]]
final class Vec3Impl[T <: Scalar: Value](val block: ExpressionBlock[Vec3[T]]) extends Vec3[T] with ExpressionHolder[Vec3[T]]
final class Vec4Impl[T <: Scalar: Value](val block: ExpressionBlock[Vec4[T]]) extends Vec4[T] with ExpressionHolder[Vec4[T]]

final class Mat2x2Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat2x2[T]]) extends Mat2x2[T] with ExpressionHolder[Mat2x2[T]]
final class Mat2x3Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat2x3[T]]) extends Mat2x3[T] with ExpressionHolder[Mat2x3[T]]
final class Mat2x4Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat2x4[T]]) extends Mat2x4[T] with ExpressionHolder[Mat2x4[T]]
final class Mat3x2Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat3x2[T]]) extends Mat3x2[T] with ExpressionHolder[Mat3x2[T]]
final class Mat3x3Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat3x3[T]]) extends Mat3x3[T] with ExpressionHolder[Mat3x3[T]]
final class Mat3x4Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat3x4[T]]) extends Mat3x4[T] with ExpressionHolder[Mat3x4[T]]
final class Mat4x2Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat4x2[T]]) extends Mat4x2[T] with ExpressionHolder[Mat4x2[T]]
final class Mat4x3Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat4x3[T]]) extends Mat4x3[T] with ExpressionHolder[Mat4x3[T]]
final class Mat4x4Impl[T <: Scalar: Value](val block: ExpressionBlock[Mat4x4[T]]) extends Mat4x4[T] with ExpressionHolder[Mat4x4[T]]
