package io.computenode.cyfra.foton.library

import io.computenode.cyfra.core.expression.Expression
import io.computenode.cyfra.core.expression.ExpressionBlock
import io.computenode.cyfra.core.expression.JumpTarget.BreakTarget
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.direct.GIO
import izumi.reflect.Tag

import scala.util.boundary.Break

class GSeq[A: Value](
  variables: List[Expression.VariableDeclare[?]],
  breakTarget: BreakTarget,
  fns: List[(ExpressionBlock[?], BreakTarget) => ExpressionBlock[?]],
):
  def scanLeft[B: Value](zero: B)(fn: (B, A) => B): GSeq[B] = ???

  def map[B: Value](fn: A => B): GSeq[B] = ???

  def filter(fn: A => Bool): GSeq[A] = ???

  def takeWhile(fn: A => Bool): GSeq[A] = ???

  def limit(n: Int): GSeq[A] = ???

  def fold[B: Value](zero: B, fn: (B, A) => B): B = ???

  def count: Int32 =
    fold(0, (acc: Int32, _: A) => acc + 1)

  def lastOr(t: A): A =
    fold(t, (_: A, elem: A) => elem)

object GSeq:
  def apply(): GSeq[Unit] = new GSeq(Nil, BreakTarget(), Nil)

  def gen[T: Value](first: T, next: T => T): GSeq[T] = GSeq().scanLeft(first)((t, unit) => next(t))

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
