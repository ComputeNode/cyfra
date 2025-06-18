package io.computenode.cyfra.dsl.collections

import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.algebra.ScalarAlgebra.{*, given}
import io.computenode.cyfra.dsl.collections.GSeq.*
import io.computenode.cyfra.dsl.control.Scope
import io.computenode.cyfra.dsl.control.When.*
import io.computenode.cyfra.dsl.macros.Source
import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

import java.util.Base64
import scala.util.Random

class GSeq[T <: Value: Tag: FromExpr](
  val uninitSource: Expression[?] => GSeqStream[?],
  val elemOps: List[GSeq.ElemOp[?]],
  val limit: Option[Int],
  val name: Source,
  val currentElemExprTreeId: Int = treeidState.getAndIncrement(),
  val aggregateElemExprTreeId: Int = treeidState.getAndIncrement(),
):

  def copyWithDynamicTrees[R <: Value: Tag: FromExpr](
    elemOps: List[GSeq.ElemOp[?]] = elemOps,
    limit: Option[Int] = limit,
    currentElemExprTreeId: Int = currentElemExprTreeId,
    aggregateElemExprTreeId: Int = aggregateElemExprTreeId,
  ) = GSeq[R](uninitSource, elemOps, limit, name, currentElemExprTreeId, aggregateElemExprTreeId)

  private val currentElemExpr = CurrentElem[T](currentElemExprTreeId)
  val source = uninitSource(currentElemExpr)
  private def currentElem: T = summon[FromExpr[T]].fromExpr(currentElemExpr)
  private def aggregateElem[R <: Value: Tag: FromExpr]: R = summon[FromExpr[R]].fromExpr(AggregateElem[R](aggregateElemExprTreeId))

  def map[R <: Value: Tag: FromExpr](fn: T => R): GSeq[R] =
    this.copyWithDynamicTrees[R](elemOps = elemOps :+ GSeq.MapOp[T, R](fn(currentElem).tree))

  def filter(fn: T => GBoolean): GSeq[T] =
    this.copyWithDynamicTrees(elemOps = elemOps :+ GSeq.FilterOp(fn(currentElem).tree))

  def takeWhile(fn: T => GBoolean): GSeq[T] =
    this.copyWithDynamicTrees(elemOps = elemOps :+ GSeq.TakeUntilOp(fn(currentElem).tree))

  def limit(n: Int): GSeq[T] =
    this.copyWithDynamicTrees(limit = Some(n))

  def fold[R <: Value: Tag: FromExpr](zero: R, fn: (R, T) => R): R =
    summon[FromExpr[R]].fromExpr(GSeq.FoldSeq(zero, fn(aggregateElem, currentElem).tree, this))

  def count: Int32 =
    fold(0, (acc: Int32, _: T) => acc + 1)

  def lastOr(t: T): T =
    fold(t, (_: T, elem: T) => elem)

object GSeq:

  def gen[T <: Value: Tag: FromExpr](first: T, next: T => T)(using name: Source) =
    GSeq(ce => GSeqStream(first, next(summon[FromExpr[T]].fromExpr(ce.asInstanceOf[E[T]])).tree), Nil, None, name)

  // REALLY naive implementation, should be replaced with dynamic array (O(1)) access
  def of[T <: Value: Tag: FromExpr](xs: List[T]) =
    GSeq
      .gen[Int32](0, _ + 1)
      .map { i =>
        val first = when(i === 0):
          xs(0)
        (if xs.length == 1 then first
         else
           xs.init.zipWithIndex.tail.foldLeft(first) { case (acc, (x, j)) =>
             acc.elseWhen(i === j) {
               x
             }
           }
        ).otherwise(xs.last)
      }
      .limit(xs.length)

  case class CurrentElem[T <: Value: Tag](tid: Int) extends PhantomExpression[T] with CustomTreeId:
    override val treeid: Int = tid

  case class AggregateElem[T <: Value: Tag](tid: Int) extends PhantomExpression[T] with CustomTreeId:
    override val treeid: Int = tid

  sealed trait ElemOp[T <: Value: Tag]:
    def tag: Tag[T] = summon[Tag[T]]
    def fn: Expression[?]

  case class MapOp[T <: Value: Tag, R <: Value: Tag](fn: Expression[?]) extends ElemOp[R]
  case class FilterOp[T <: Value: Tag](fn: Expression[GBoolean]) extends ElemOp[T]
  case class TakeUntilOp[T <: Value: Tag](fn: Expression[GBoolean]) extends ElemOp[T]

  sealed trait GSeqSource[T <: Value: Tag]
  case class GSeqStream[T <: Value: Tag](init: T, next: Expression[?]) extends GSeqSource[T]

  case class FoldSeq[R <: Value: Tag, T <: Value: Tag](zero: R, fn: Expression[?], seq: GSeq[T]) extends Expression[R]:
    val zeroExpr = zero.tree
    val fnExpr = fn
    val streamInitExpr = seq.source.init.tree
    val streamNextExpr = seq.source.next
    val seqExprs = seq.elemOps.map(_.fn)

    val limitExpr = ConstInt32(seq.limit.getOrElse(throw new IllegalArgumentException("Reduce on infinite stream is not supported")))

    override val exprDependencies: List[E[?]] = List(zeroExpr, streamInitExpr, limitExpr)
    override val introducedScopes: List[Scope[?]] = Scope(fnExpr)(using fnExpr.tag) :: Scope(streamNextExpr)(using streamNextExpr.tag) ::
      seqExprs.map(e => Scope(e)(using e.tag))
