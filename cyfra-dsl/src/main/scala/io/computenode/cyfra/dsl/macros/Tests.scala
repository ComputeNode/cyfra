package io.computenode.cyfra.dsl.macros

import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.Algebra.*
import io.computenode.cyfra.dsl.Value.given
import io.computenode.cyfra.dsl.Algebra.given
import io.computenode.cyfra.dsl.*

object Tests:
  @main
  def test() = 
    println("Hello, ss!")

    def xd(x: Any): Any = x

    def purefn(i: Int32, j: Int32): Int32 =
      val a = 2
      val b = 3
      val foo = summon[Source]
      println(foo)
      3

    purefn(2, 3)