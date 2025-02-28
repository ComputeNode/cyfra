package io.computenode.cyfra.samples

import io.computenode.cyfra.dsl.Value.Float32
import io.computenode.cyfra.dsl.Value.given
import io.computenode.cyfra.dsl.Algebra.*
import io.computenode.cyfra.dsl.Algebra.given

object PlaygroundMain:

  def main(args: Array[String]): Unit = // Use a traditional main method
    val exampleFloat: Float32 = 3.14f
    println("HI")
    println(s"Example float: ${exampleFloat.toString}")