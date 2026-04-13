package io.computenode.cyfra.core.expression.types

import io.computenode.cyfra.core.expression.*
import izumi.reflect.Tag

private[types] def const[A: Value](value: Any): A =
  Value[A].extract(ExpressionBlock(Expression.Constant[A](value)))

sealed trait Scalar

sealed trait NumericalType extends Scalar
sealed trait NegativeType extends NumericalType

trait FloatType extends NegativeType

sealed trait IntegerType extends NumericalType
trait SignedIntType extends IntegerType with NegativeType
trait UnsignedIntType extends IntegerType

abstract class Bool extends Scalar
object Bool:
  def apply(value: Boolean): Bool = const(value)
  given Value.Scalar[Bool] with
    protected def extractUnsafe(ir: ExpressionBlock[Bool]): Bool = new BoolImpl(ir)
    def tag: Tag[Bool] = Tag[Bool]

abstract class Float16 extends FloatType
object Float16:
  def apply(value: Float): Float16 = const(value)
  given Value.Scalar[Float16] with
    protected def extractUnsafe(ir: ExpressionBlock[Float16]): Float16 = new Float16Impl(ir)
    def tag: Tag[Float16] = Tag[Float16]

abstract class Float32 extends FloatType
object Float32:
  def apply(value: Float): Float32 = const(value)
  given Value.Scalar[Float32] with
    protected def extractUnsafe(ir: ExpressionBlock[Float32]): Float32 = new Float32Impl(ir)
    def tag: Tag[Float32] = Tag[Float32]

abstract class Int16 extends SignedIntType
object Int16:
  def apply(value: Int): Int16 = const(value)
  given Value.Scalar[Int16] with
    protected def extractUnsafe(ir: ExpressionBlock[Int16]): Int16 = new Int16Impl(ir)
    def tag: Tag[Int16] = Tag[Int16]

abstract class Int32 extends SignedIntType
object Int32:
  def apply(value: Int): Int32 = const(value)
  given Value.Scalar[Int32] with
    protected def extractUnsafe(ir: ExpressionBlock[Int32]): Int32 = new Int32Impl(ir)
    def tag: Tag[Int32] = Tag[Int32]

abstract class UInt16 extends UnsignedIntType
object UInt16:
  def apply(value: Int): UInt16 = const(value)
  given Value.Scalar[UInt16] with
    protected def extractUnsafe(ir: ExpressionBlock[UInt16]): UInt16 = new UInt16Impl(ir)
    def tag: Tag[UInt16] = Tag[UInt16]

abstract class UInt32 extends UnsignedIntType
object UInt32:
  def apply(value: Int): UInt32 = const(value)
  given Value.Scalar[UInt32] with
    protected def extractUnsafe(ir: ExpressionBlock[UInt32]): UInt32 = new UInt32Impl(ir)
    def tag: Tag[UInt32] = Tag[UInt32]
