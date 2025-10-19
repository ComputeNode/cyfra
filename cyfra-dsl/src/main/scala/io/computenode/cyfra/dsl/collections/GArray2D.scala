package io.computenode.cyfra.dsl.collections

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.algebra.ScalarAlgebra.{*, given}
import io.computenode.cyfra.dsl.macros.Source
import izumi.reflect.Tag
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.GBuffer

// todo temporary
class GArray2D[T <: Value: {Tag, FromExpr}](width: Int, val arr: GBuffer[T]):
  def at(x: Int32, y: Int32)(using Source): T =
    arr.read(y * width + x)
