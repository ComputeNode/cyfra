package io.computenode.cyfra.dsl

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.dsl.direct.GIO

object Library:
  def invocationId: UInt32 = Value.map(BuildInFunction.GlobalInvocationId)

  def when[A: Value](cond: Bool)(ifTrue: => A)(ifFalse: => A): A =
    val exp = GIO.reify:
      val tBlock: GIO ?=> A =
        ifTrue
      val fBlock: GIO ?=> A =
        ifFalse
      GIO.branch[A](cond, tBlock, fBlock)
    Value[A].extract(exp)
  


