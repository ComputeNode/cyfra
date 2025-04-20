package io.computenode.cyfra.samples.slides

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.{FloatMem, GArray, GContext, GFunction, MVPContext}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.{given}

import scala.concurrent.Await
import scala.concurrent.duration.given

given GContext = new MVPContext()

// @main
// def sample =
//   val gpuFunction = GFunction:
//     (value: Float32) => value * 2f

//   val data = FloatMem((1 to 128).map(_.toFloat).toArray)

//   val result = Await.result(data.map(gpuFunction), 1.second)
//   println(result.mkString(", "))


