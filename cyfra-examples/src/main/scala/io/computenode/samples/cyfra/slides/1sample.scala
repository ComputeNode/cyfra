package io.computenode.samples.cyfra.slides

import io.computenode.cyfra.given

import scala.concurrent.Await
import scala.concurrent.duration.given
import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.given
import io.computenode.cyfra.runtime.*
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.given
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.runtime.mem.FloatMem

given GContext = new MVPContext()

@main
def sample =
  val gpuFunction = GFunction:
    (value: Float32) => value * 2f

  val data = FloatMem((1 to 128).map(_.toFloat).toArray)

  val result = data.map(gpuFunction).asInstanceOf[FloatMem].toArray
  println(result.mkString(", "))
  