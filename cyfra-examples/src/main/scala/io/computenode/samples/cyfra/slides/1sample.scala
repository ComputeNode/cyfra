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
  val data = FloatMem((1 to 256).map(_.toFloat).toArray)

  val gpuFunction = GFunction:
    (value: Float32) => value * 2f

  val result = data.map(gpuFunction).asInstanceOf[FloatMem].toArray
  println(result.mkString(", "))
  