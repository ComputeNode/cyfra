package io.computenode.cyfra.dsl
import io.computenode.cyfra.dsl.Control.Scope
import io.computenode.cyfra.dsl.Expression
import Expression.{Const, treeidState}
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.macros.FnCall.FnIdentifier
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag

import java.util.concurrent.atomic.AtomicInteger

trait Expression[T <: Value: Tag] extends Product:
  def tag: Tag[T] = summon[Tag[T]]
  private[cyfra] val treeid: Int = treeidState.getAndIncrement()
  private[cyfra] var of: Option[Value] = None
  private lazy val childrenStrings = this.exprDependencies
    .map(e => s"#${e.treeid}")
    .mkString("[", ", ", "]")
  override def toString: String = s"${this.productPrefix}(${of.fold("")(v => s"name = ${v.source}, ")}children=$childrenStrings, id=$treeid)"
  private def exploreDeps(children: List[Any]): (List[Expression[?]], List[Scope[?]]) = (for (elem <- children) yield elem match {
    case b: Scope[?] =>
      (None, Some(b))
    case x: Expression[?] =>
      (Some(x), None)
    case x: Value =>
      (Some(x.tree), None)
    case list: List[Any] =>
      (exploreDeps(list.collect { case v: Value => v })._1, exploreDeps(list.collect { case s: Scope[?] => s })._2)
    case _ => (None, None)
  }).foldLeft((List.empty[Expression[?]], List.empty[Scope[?]])) { case ((acc, blockAcc), (newExprs, newBlocks)) =>
    (acc ::: newExprs.iterator.toList, blockAcc ::: newBlocks.iterator.toList)
  }
  def exprDependencies: List[Expression[?]] = exploreDeps(this.productIterator.toList)._1
  def introducedScopes: List[Scope[?]] = exploreDeps(this.productIterator.toList)._2

object Expression:
  trait CustomTreeId:
    self: Expression[?] =>
    override val treeid: Int

  trait PhantomExpression[T <: Value: Tag] extends Expression[T]

  private[cyfra] val treeidState: AtomicInteger = new AtomicInteger(0)

  type E[T <: Value] = Expression[T]

  case class Negate[T <: Value: Tag](a: T) extends Expression[T]
  sealed trait BinaryOpExpression[T <: Value: Tag] extends Expression[T] {
    def a: T
    def b: T
  }
  case class Sum[T <: Value: Tag](a: T, b: T) extends BinaryOpExpression[T]
  case class Diff[T <: Value: Tag](a: T, b: T) extends BinaryOpExpression[T]
  case class Mul[T <: Scalar: Tag](a: T, b: T) extends BinaryOpExpression[T]
  case class Div[T <: Scalar: Tag](a: T, b: T) extends BinaryOpExpression[T]
  case class Mod[T <: Scalar: Tag](a: T, b: T) extends BinaryOpExpression[T]
  case class ScalarProd[S <: Scalar, V <: Vec[S]: Tag](a: V, b: S) extends Expression[V]
  case class DotProd[S <: Scalar: Tag, V <: Vec[S]](a: V, b: V) extends Expression[S]

  sealed trait BitwiseOpExpression[T <: Scalar: Tag] extends Expression[T]
  sealed trait BitwiseBinaryOpExpression[T <: Scalar: Tag] extends BitwiseOpExpression[T] {
    def a: T
    def b: T
  }
  case class BitwiseAnd[T <: Scalar: Tag](a: T, b: T) extends BitwiseBinaryOpExpression[T]
  case class BitwiseOr[T <: Scalar: Tag](a: T, b: T) extends BitwiseBinaryOpExpression[T]
  case class BitwiseXor[T <: Scalar: Tag](a: T, b: T) extends BitwiseBinaryOpExpression[T]
  case class BitwiseNot[T <: Scalar: Tag](a: T) extends BitwiseOpExpression[T]
  case class ShiftLeft[T <: Scalar: Tag](a: T, by: UInt32) extends BitwiseOpExpression[T]
  case class ShiftRight[T <: Scalar: Tag](a: T, by: UInt32) extends BitwiseOpExpression[T]

  sealed trait ComparisonOpExpression[T <: Value: Tag] extends Expression[GBoolean] {
    def operandTag = summon[Tag[T]]
    def a: T
    def b: T
  }
  case class GreaterThan[T <: Scalar: Tag](a: T, b: T) extends ComparisonOpExpression[T]
  case class LessThan[T <: Scalar: Tag](a: T, b: T) extends ComparisonOpExpression[T]
  case class GreaterThanEqual[T <: Scalar: Tag](a: T, b: T) extends ComparisonOpExpression[T]
  case class LessThanEqual[T <: Scalar: Tag](a: T, b: T) extends ComparisonOpExpression[T]
  case class Equal[T <: Scalar: Tag](a: T, b: T) extends ComparisonOpExpression[T]

  case class And(a: GBoolean, b: GBoolean) extends Expression[GBoolean]
  case class Or(a: GBoolean, b: GBoolean) extends Expression[GBoolean]
  case class Not(a: GBoolean) extends Expression[GBoolean]

  case class ExtractScalar[V <: Vec[?]: Tag, S <: Scalar: Tag](a: V, i: Int32) extends Expression[S]

  sealed trait ConvertExpression[F <: Scalar: Tag, T <: Scalar: Tag] extends Expression[T] {
    def fromTag: Tag[F] = summon[Tag[F]]
    def a: F
  }
  case class ToFloat32[T <: Scalar: Tag](a: T) extends ConvertExpression[T, Float32]
  case class ToInt32[T <: Scalar: Tag](a: T) extends ConvertExpression[T, Int32]
  case class ToUInt32[T <: Scalar: Tag](a: T) extends ConvertExpression[T, UInt32]

  sealed trait Const[T <: Scalar: Tag] extends Expression[T] {
    def value: Any
  }
  object Const {
    def unapply[T <: Scalar](c: Const[T]): Option[Any] = Some(c.value)
  }

  case class ConstFloat32(value: Float) extends Const[Float32]
  case class ConstInt32(value: Int) extends Const[Int32]
  case class ConstUInt32(value: Int) extends Const[UInt32]
  case class ConstGB(value: Boolean) extends Const[GBoolean]

  case class ComposeVec2[T <: Scalar: Tag](a: T, b: T) extends Expression[Vec2[T]]
  case class ComposeVec3[T <: Scalar: Tag](a: T, b: T, c: T) extends Expression[Vec3[T]]
  case class ComposeVec4[T <: Scalar: Tag](a: T, b: T, c: T, d: T) extends Expression[Vec4[T]]

  case class ExtFunctionCall[R <: Value: Tag](fn: FunctionName, args: List[Value]) extends Expression[R]
  case class FunctionCall[R <: Value: Tag](fn: FnIdentifier, body: Scope[R], args: List[Value]) extends E[R]

  case class Pass[T <: Value: Tag](value: T) extends E[T]

  case class Dynamic[T <: Value: Tag](source: String) extends E[T]
