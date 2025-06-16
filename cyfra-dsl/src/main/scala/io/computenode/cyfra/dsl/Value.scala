package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Expression.{E, E as T}
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag

trait Value:
  def tree: E[?]
  def source: Source
  private[cyfra] def treeid: Int = tree.treeid
  protected def init() =
    tree.of = Some(this)
  init()

object Value:
  
  trait FromExpr[T <: Value]:
    def fromExpr(expr: E[T])(using name: Source): T

  sealed trait Scalar extends Value

  trait FloatType extends Scalar
  case class Float32(tree: E[Float32])(using val source: Source) extends FloatType
  given FromExpr[Float32] with
    def fromExpr(f: E[Float32])(using Source) = Float32(f)

  trait IntType extends Scalar
  case class Int32(tree: E[Int32])(using val source: Source) extends IntType
  given FromExpr[Int32] with
    def fromExpr(f: E[Int32])(using Source) = Int32(f)

  trait UIntType extends Scalar
  case class UInt32(tree: E[UInt32])(using val source: Source) extends UIntType
  given FromExpr[UInt32] with
    def fromExpr(f: E[UInt32])(using Source) = UInt32(f)

  case class GBoolean(tree: E[GBoolean])(using val source: Source) extends Scalar
  given FromExpr[GBoolean] with
    def fromExpr(f: E[GBoolean])(using Source) = GBoolean(f)

  sealed trait Vec[T <: Value] extends Value

  case class Vec2[T <: Value](tree: E[Vec2[T]])(using val source: Source) extends Vec[T]
  given [T <: Scalar]: FromExpr[Vec2[T]] with
    def fromExpr(f: E[Vec2[T]])(using Source) = Vec2(f)

  case class Vec3[T <: Value](tree: E[Vec3[T]])(using val source: Source) extends Vec[T]
  given [T <: Scalar]: FromExpr[Vec3[T]] with
    def fromExpr(f: E[Vec3[T]])(using Source) = Vec3(f)

  case class Vec4[T <: Value](tree: E[Vec4[T]])(using val source: Source) extends Vec[T]
  given [T <: Scalar]: FromExpr[Vec4[T]] with
    def fromExpr(f: E[Vec4[T]])(using Source) = Vec4(f)
