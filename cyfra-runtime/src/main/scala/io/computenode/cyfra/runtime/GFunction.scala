package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import izumi.reflect.Tag

case class GFunction[
  G <: GStruct[G] : GStructSchema : Tag, 
  H <: Value : Tag : FromExpr, 
  R <: Value : Tag : FromExpr
](fn: (G, Int32, GArray[H]) => R)(implicit context: GContext){
  def arrayInputs: List[Tag[_]] = List(summon[Tag[H]])
  def arrayOutputs: List[Tag[_]] = List(summon[Tag[R]])
  val pipeline: ComputePipeline = context.compile(this)
}

object GFunction:
  def apply[
    H <: Value : Tag : FromExpr,
    R <: Value : Tag : FromExpr
  ](fn: H => R)(using context: GContext) =
    new GFunction[GStruct.Empty, H, R](
      (_, index: Int32, gArray: GArray[H]) => fn(gArray.at(index))
    )

  def from2D[
    G <: GStruct[G] : GStructSchema : Tag,
    H <: Value : Tag : FromExpr,
    R <: Value : Tag : FromExpr
  ](width: Int)(fn: (G, (Int32, Int32), GArray2D[H]) => R)(using context: GContext) =
    GFunction[G, H, R](
      (g: G, index: Int32, a: GArray[H]) =>
        val x: Int32 = index / width
        val y: Int32 = index mod width
        val arr = GArray2D(width, ???, a)
        fn(g, (x, y), arr)
    )
