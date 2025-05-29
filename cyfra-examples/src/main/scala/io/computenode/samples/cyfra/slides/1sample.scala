package io.computenode.samples.cyfra.slides

import io.computenode.cyfra.given

import scala.concurrent.Await
import scala.concurrent.duration.given
import io.computenode.cyfra.given
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.runtime.mem.FloatMem

given GContext = new GContext()

@main
def sample =
  val data = (0 to 256).map(_.toFloat).toArray
  val mem = FloatMem(data)

  val gpuFunction = GFunction:
    (value: Float32) => value * 2f

  val result = mem.map(gpuFunction).toFloatArray
  println(result.mkString(", "))
  