package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Algebra.FromExpr
import io.computenode.cyfra.dsl.{GArray2D, GStruct, GStructSchema, Value}
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.vulkan.compute.ComputePipeline
import izumi.reflect.Tag

case class GFunction[
  G <: GStruct[G] : GStructSchema : Tag, 
  H <: Value : Tag : FromExpr, 
  R <: Value : Tag : FromExpr
](
  width: Int,
  height: Int,
  fn: (G, (Int32, Int32), GArray2D[H]) => R
)(implicit context: GContext){
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
      ???,
      ???,
      (_, xy: (Int32, Int32), gArray: GArray2D[H])
        => fn(gArray.at(xy._1, xy._2))
    )
