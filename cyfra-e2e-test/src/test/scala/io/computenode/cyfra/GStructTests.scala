package io.computenode.cyfra.e2e

import io.computenode.cyfra.runtime.*, mem.*
import io.computenode.cyfra.dsl.{*, given}
import GStruct.Empty.given

class GStructE2eTest extends munit.FunSuite:
  given gc: GContext = GContext()

  test("custom GStruct"): // STUBS
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      ???

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray
    result.foreach(println)
  
  test("GStruct of GStructs"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      ???

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray
    result.foreach(println)

  test("GSeq of GStructs"):
    val gf: GFunction[GStruct.Empty, Float32, Float32] = GFunction: f =>
      ???

    val inArr = (0 to 255).map(_.toFloat).toArray
    val gmem = FloatMem(inArr)
    val result = gmem.map(gf).asInstanceOf[FloatMem].toArray
    result.foreach(println)
