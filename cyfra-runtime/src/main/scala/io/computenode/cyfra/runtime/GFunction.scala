package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.{*, given} 
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.Expression.E 
import izumi.reflect.Tag

case class GFunction[
  G <: GStruct[G] : GStructSchema : Tag,
  H <: Value : Tag : FromExpr,
  R <: Value : Tag : FromExpr
](
  val fn: (G, Int32, GArray[H]) => R 
) {
  def arrayInputs: List[Tag[_]] = List(summon[Tag[H]])
  def arrayOutputs: List[Tag[_]] = List(summon[Tag[R]])
}

object GFunction:
  def apply[
    H <: Value : Tag : FromExpr,
    R <: Value : Tag : FromExpr
  ](userSimpleFn: H => R): GFunction[GStruct.Empty, H, R] =
    new GFunction[GStruct.Empty, H, R](
      (_: GStruct.Empty, workerIdx: Int32, gArray: GArray[H]) => userSimpleFn(gArray.at(workerIdx))
    )

  def from2D[
    G <: GStruct[G] : GStructSchema : Tag,
    H <: Value : Tag : FromExpr,
    R <: Value : Tag : FromExpr
  ](width: Int, userFn2D: (G, (Int32, Int32), GArray2D[H]) => R): GFunction[G, H, R] =
    new GFunction[G, H, R](
      (g: G, index: Int32, garray: GArray[H]) =>
        val x: Int32 = index mod width
        val y: Int32 = index / width
        val arr2d = GArray2D(width, garray)
        userFn2D(g, (x, y), arr2d)
    )
