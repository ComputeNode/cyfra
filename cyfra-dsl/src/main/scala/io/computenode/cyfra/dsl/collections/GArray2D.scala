package io.computenode.cyfra.dsl.collections

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.algebra.ScalarAlgebra.{*, given}
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag
import io.computenode.cyfra.dsl.Value.FromExpr

class GArray2D[T <: Value: Tag: FromExpr](width: Int, val arr: GArray[T]) {
  def at(x: Int32, y: Int32)(using Source): T =
    arr.at(y * width + x)
}
