package io.computenode.cyfra.samples.slides

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.aalegacy.*
import io.computenode.cyfra.core.aalegacy.mem.FloatMem

given GContext = new GContext()

@main
def sample() =
  val gpuFunction = GFunction: (value: Float32) =>
    value * 2f

  val data = FloatMem((1 to 128).map(_.toFloat).toArray)

  val result = data.map(gpuFunction).asInstanceOf[FloatMem].toArray
  println(result.mkString(", "))
