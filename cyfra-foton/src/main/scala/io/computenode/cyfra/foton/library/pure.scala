package io.computenode.cyfra.foton.library

import io.computenode.cyfra.core.expression.CustomFunction.CustomFunction1
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.CustomFunction
import io.computenode.cyfra.dsl.direct.GIO

class pure[A: Value, B: Value](f: CustomFunction1[B, A]):
  def apply(a: A): B =
    val exp = GIO.reify:
      val aVar = GIO.declare[A]()
      GIO.write(aVar, a)
      GIO.call(f, aVar)
    Value[B].extract(exp)

object pure:
  def apply[A: Value, B: Value](f: A => B): pure[A, B] =
    val cf = CustomFunction[A, B]: variable =>
      GIO.reify:
        val a = GIO.read(variable)
        f(a)
    new pure(cf)
