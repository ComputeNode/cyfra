package io.computenode.cyfra.core.expression

import izumi.reflect.Tag

given Value[Float16] with
  protected def extractUnsafe(ir: ExpressionBlock[Float16]): Float16 = new Float16Impl(ir)
  def tag: Tag[Float16] = Tag[Float16]

given Value[Float32] with
  protected def extractUnsafe(ir: ExpressionBlock[Float32]): Float32 = new Float32Impl(ir)
  def tag: Tag[Float32] = Tag[Float32]

given Value[Int16] with
  protected def extractUnsafe(ir: ExpressionBlock[Int16]): Int16 = new Int16Impl(ir)
  def tag: Tag[Int16] = Tag[Int16]

given Value[Int32] with
  protected def extractUnsafe(ir: ExpressionBlock[Int32]): Int32 = new Int32Impl(ir)
  def tag: Tag[Int32] = Tag[Int32]

given Value[UInt16] with
  protected def extractUnsafe(ir: ExpressionBlock[UInt16]): UInt16 = new UInt16Impl(ir)
  def tag: Tag[UInt16] = Tag[UInt16]

given Value[UInt32] with
  protected def extractUnsafe(ir: ExpressionBlock[UInt32]): UInt32 = new UInt32Impl(ir)
  def tag: Tag[UInt32] = Tag[UInt32]

given Value[Bool] with
  protected def extractUnsafe(ir: ExpressionBlock[Bool]): Bool = new BoolImpl(ir)
  def tag: Tag[Bool] = Tag[Bool]

val unitZero = Expression.Constant[Unit](())
given Value[Unit] with
  protected def extractUnsafe(ir: ExpressionBlock[Unit]): Unit = ()
  def tag: Tag[Unit] = Tag[Unit]

given Value[Any] with
  protected def extractUnsafe(ir: ExpressionBlock[Any]): Any = ir.result.asInstanceOf[Expression.Constant[Any]].value
  def tag: Tag[Any] = Tag[Any]

given [T <: Scalar: Value]: Value[Vec2[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Vec2[T]]): Vec2[T] = new Vec2Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Vec2[T]] = Tag[Vec2[T]]

given [T <: Scalar: Value]: Value[Vec3[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Vec3[T]]): Vec3[T] = new Vec3Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Vec3[T]] = Tag[Vec3[T]]

given [T <: Scalar: Value]: Value[Vec4[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Vec4[T]]): Vec4[T] = new Vec4Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Vec4[T]] = Tag[Vec4[T]]

given [T <: Scalar: Value]: Value[Mat2x2[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat2x2[T]]): Mat2x2[T] = new Mat2x2Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat2x2[T]] = Tag[Mat2x2[T]]

given [T <: Scalar: Value]: Value[Mat2x3[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat2x3[T]]): Mat2x3[T] = new Mat2x3Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat2x3[T]] = Tag[Mat2x3[T]]

given [T <: Scalar: Value]: Value[Mat2x4[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat2x4[T]]): Mat2x4[T] = new Mat2x4Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat2x4[T]] = Tag[Mat2x4[T]]

given [T <: Scalar: Value]: Value[Mat3x2[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat3x2[T]]): Mat3x2[T] = new Mat3x2Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat3x2[T]] = Tag[Mat3x2[T]]

given [T <: Scalar: Value]: Value[Mat3x3[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat3x3[T]]): Mat3x3[T] = new Mat3x3Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat3x3[T]] = Tag[Mat3x3[T]]

given [T <: Scalar: Value]: Value[Mat3x4[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat3x4[T]]): Mat3x4[T] = new Mat3x4Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat3x4[T]] = Tag[Mat3x4[T]]

given [T <: Scalar: Value]: Value[Mat4x2[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat4x2[T]]): Mat4x2[T] = new Mat4x2Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat4x2[T]] = Tag[Mat4x2[T]]

given [T <: Scalar: Value]: Value[Mat4x3[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat4x3[T]]): Mat4x3[T] = new Mat4x3Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat4x3[T]] = Tag[Mat4x3[T]]

given [T <: Scalar: Value]: Value[Mat4x4[T]] with
  protected def extractUnsafe(ir: ExpressionBlock[Mat4x4[T]]): Mat4x4[T] = new Mat4x4Impl[T](ir)
  given Tag[T] = summon[Value[T]].tag
  def tag: Tag[Mat4x4[T]] = Tag[Mat4x4[T]]
