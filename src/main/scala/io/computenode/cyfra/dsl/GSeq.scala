package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Control.{Scope, when}
import io.computenode.cyfra.dsl.Expression.{ConstInt32, E}
import io.computenode.cyfra.dsl.Value.*
import izumi.reflect.Tag
import org.slf4j.{Logger, LoggerFactory}

import java.util.concurrent.atomic.AtomicInteger

class GSeq[T <: Value: Tag: FromExpr](
  val uninitSource: Expression[_] => GSeq.GSeqStream[_], // Fixed: Use GSeq.GSeqStream
  val elemOps: List[GSeq.ElemOp[_]],
  val limit: Option[Int],
  val name: sourcecode.Name,
  val currentElemExprTreeId: Int = GSeq.treeidState.getAndIncrement(),
  val aggregateElemExprTreeId: Int = GSeq.treeidState.getAndIncrement()
):

  def copyWithDynamicTrees[R <: Value: Tag: FromExpr](
    elemOps: List[GSeq.ElemOp[_]] = elemOps,
    limit: Option[Int] = limit,
    currentElemExprTreeId: Int = currentElemExprTreeId,
    aggregateElemExprTreeId: Int = aggregateElemExprTreeId
  ): GSeq[R] = GSeq[R](
    uninitSource,
    elemOps,
    limit,
    name,
    currentElemExprTreeId,
    aggregateElemExprTreeId
  )

  private val currentElemExpr = GSeq.CurrentElem[T](currentElemExprTreeId)
  val source: GSeq.GSeqStream[_] = uninitSource(currentElemExpr) // Fixed: Use GSeq.GSeqStream

  private def currentElem: T = summon[FromExpr[T]].fromExpr(currentElemExpr)
  private def aggregateElem[R <: Value: Tag: FromExpr]: R = summon[FromExpr[R]].fromExpr(GSeq.AggregateElem[R](aggregateElemExprTreeId))

  def map[R <: Value: Tag: FromExpr](fn: T => R): GSeq[R] =
    this.copyWithDynamicTrees[R](elemOps = elemOps :+ GSeq.MapOp[T, R](
      fn(currentElem).tree
    ))

  def filter(fn: T => GBoolean): GSeq[T] =
    this.copyWithDynamicTrees(elemOps = elemOps :+ GSeq.FilterOp(
      fn(currentElem).tree
    ))

  def takeWhile(fn: T => GBoolean): GSeq[T] =
    this.copyWithDynamicTrees(elemOps = elemOps :+ GSeq.TakeUntilOp(
      fn(currentElem).tree
    ))

  def limit(n: Int): GSeq[T] =
    this.copyWithDynamicTrees(limit = Some(n))

  def fold[R <: Value: Tag: FromExpr](zero: R, fn: (R, T) => R): R =
    summon[FromExpr[R]].fromExpr(GSeq.FoldSeq(zero, fn(aggregateElem, currentElem).tree, this))

  def count: Int32 =
    fold(0, (acc: Int32, _: T) => acc + 1)

  def lastOr(t: T): T =
    fold(t, (_: T, elem: T) => elem)

object GSeq:
  private val treeidState = new AtomicInteger(0)
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def gen[T <: Value: Tag: FromExpr](first: T, next: T => T)(using name: sourcecode.Name): GSeq[T] = GSeq(
    ce => GSeqStream(first, next(summon[FromExpr[T]].fromExpr(ce.asInstanceOf[E[T]])).tree),
    Nil,
    None,
    name
  )

  // REALLY naive implementation, should be replaced with dynamic array (O(1)) access
  def of[T <: Value: Tag: FromExpr](xs: List[T]): GSeq[T] =
    GSeq.gen[Int32](0, _ + 1).map { i =>
      val first = when(i === 0) {
        xs(0)
      }
      (if (xs.length == 1) {
        first
      } else {
        xs.init.zipWithIndex.tail.foldLeft(first) {
          case (acc, (x, j)) =>
            acc.elseWhen(i === j) {
              x
            }
        }
      }).otherwise(xs.last)
    }.limit(xs.length)

  case class CurrentElem[T <: Value: Tag](tid: Int) extends PhantomExpression[T] with CustomTreeId:
    override val treeid: Int = tid

  case class AggregateElem[T <: Value: Tag](tid: Int) extends PhantomExpression[T] with CustomTreeId:
    override val treeid: Int = tid

  sealed trait ElemOp[T <: Value: Tag]:
    def tag: Tag[T] = summon[Tag[T]]
    def fn: Expression[_]

  case class MapOp[T <: Value: Tag, R <: Value: Tag](fn: Expression[_]) extends ElemOp[R]
  case class FilterOp[T <: Value: Tag](fn: Expression[GBoolean]) extends ElemOp[T]
  case class TakeUntilOp[T <: Value: Tag](fn: Expression[GBoolean]) extends ElemOp[T]

  sealed trait GSeqSource[T <: Value: Tag]
  case class GSeqStream[T <: Value: Tag](init: T, next: Expression[_]) extends GSeqSource[T]

  case class FoldSeq[R <: Value: Tag, T <: Value: Tag](zero: R, fn: Expression[_], seq: GSeq[T]) extends Expression[R]:
    val zeroExpr: Expression[_] = zero.tree
    val fnExpr: Expression[_] = fn
    val streamInitExpr: Expression[_] = seq.source.init.tree
    val streamNextExpr: Expression[_] = seq.source.next
    val seqExprs: List[Expression[_]] = seq.elemOps.map(_.fn)

    // Ensure a limit is set to avoid infinite stream errors
    val limitExpr: ConstInt32 = ConstInt32(seq.limit.getOrElse {
      logger.warn("No limit set on GSeq. This may result in an infinite stream. Call .limit(n) to set a limit.")
      throw new IllegalArgumentException("Reduce on infinite stream is not supported. Call .limit(n) first.")
    })

    override val exprDependencies: List[E[_]] = List(zeroExpr, streamInitExpr, limitExpr)
    override val introducedScopes: List[Scope[_]] = Scope(fnExpr)(using fnExpr.tag) :: Scope(streamNextExpr)(using streamNextExpr.tag) :: seqExprs.map(e => Scope(e)(using e.tag))