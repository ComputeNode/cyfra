package io.computenode.cyfra.foton.library

import io.computenode.cyfra.core.expression.JumpTarget.{BreakTarget, ContinueTarget}
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.direct.GIO

final class GSeq[A: Value] private (
  source: GSeq.Source[?],
  pipeline: Any => (BreakTarget, ContinueTarget, GIO) ?=> A,
  maxLength: Option[Int],
):
  def map[B: Value](fn: A => B): GSeq[B] =
    new GSeq(source, s => fn(pipeline(s)), maxLength)

  def filter(fn: A => Bool): GSeq[A] =
    new GSeq(
      source,
      s =>
        val a = pipeline(s)
        GIO.conditionalContinue(!fn(a))
        a,
      maxLength,
    )

  def takeWhile(fn: A => Bool): GSeq[A] =
    new GSeq(
      source,
      s =>
        val a = pipeline(s)
        GIO.conditionalBreak(!fn(a))
        a,
      maxLength,
    )

  def limit(n: Int): GSeq[A] = new GSeq(source, pipeline, Some(n))

  def fold[B: Value](zero: B, fn: (B, A) => B): B =
    val n = maxLength.getOrElse(throw IllegalArgumentException("Cannot fold an infinite GSeq, call limit first"))
    source.fold(zero, n, pipeline, fn)

  def count: Int32 =
    fold(0, (acc: Int32, _: A) => acc + 1)

  def lastOr(t: A): A =
    fold(t, (_: A, elem: A) => elem)

object GSeq:
  private class Source[S: Value](first: S, next: S => S):
    def fold[A, B: Value](zero: B, n: Int, pipeline: Any => (BreakTarget, ContinueTarget, GIO) ?=> A, fn: (B, A) => B): B =
      val exp = GIO.reify:
        val state = GIO.declare[S](Some(first))
        val acc = GIO.declare[B](Some(zero))
        val i = GIO.declare[Int32](Some(Int32(0)))
        GIO.loop(
          {
            GIO.conditionalBreak(GIO.read(i) >= n)
            val a = pipeline(GIO.read(state))
            GIO.write(acc, fn(GIO.read(acc), a))
          },
          {
            GIO.write(state, next(GIO.read(state)))
            GIO.write(i, GIO.read(i) + 1)
          },
        )
        GIO.read(acc)
      Value[B].extract(exp)

  def gen[T: Value](first: T, next: T => T): GSeq[T] =
    new GSeq[T](Source(first, next), s => s.asInstanceOf[T], None)

  // REALLY naive implementation, should be replaced with dynamic array (O(1)) access
  def of[T: Value](xs: List[T]) =
    GSeq
      .gen[Int32](0, _ + 1)
      .map: i =>
        val first = when(i === 0):
          xs.head
        (if xs.length == 1 then first
         else
           xs.init.zipWithIndex.tail.foldLeft(first):
             case (acc, (x, j)) =>
               acc.elseWhen(i === j):
                 x
        ).otherwise(xs.last)
      .limit(xs.length)
