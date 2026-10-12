package io.computenode.cyfra.dsl

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

import io.computenode.cyfra.dsl.direct.GIO

object Library:
  final class WhenBuilder[A: Value](private val cases: List[(Bool, () => A)]):

    def elseWhen(cond: Bool)(ifTrue: => A): WhenBuilder[A] =
      new WhenBuilder(cases :+ (cond -> (() => ifTrue)))

    def otherwise(ifFalse: => A): A =
      val exp = GIO.reify:

        def build(cs: List[(Bool, () => A)]): GIO ?=> A =
          cs match
            case (cond, thunk) :: tail =>
              val tBlock: GIO ?=> A =
                thunk()
              val fBlock: GIO ?=> A =
                build(tail)
              GIO.branch[A](cond, tBlock, fBlock)

            case Nil =>
              ifFalse

        build(cases)

      Value[A].extract(exp)

  def when[A: Value](cond: Bool)(ifTrue: => A): WhenBuilder[A] =
    new WhenBuilder(List(cond -> (() => ifTrue)))
  val invocationIdX: UInt32 =
    val exp = GIO.reify:
      GIO.read(GlobalInvocationId.focus(_.x))
    Value[UInt32].extractUnchecked(exp)
