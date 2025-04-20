package io.computenode.samples.cyfra

import io.computenode.cyfra.dsl.Algebra.given
import io.computenode.cyfra.dsl.Value.{Float32, given}

object Playground:

  @main
  def playground =
    val exampleFloat: Float32 = 3.14f
    println("HI")
    println(s"Example float: ${exampleFloat.name}")
