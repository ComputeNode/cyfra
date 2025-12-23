package io.computenode.cyfra.dsl.archive.collections

import io.computenode.cyfra.dsl.archive.Value.Int32
import io.computenode.cyfra.dsl.archive.algebra.ScalarAlgebra.{*, given}
import izumi.reflect.Tag
import io.computenode.cyfra.dsl.archive.Value.FromExpr
import io.computenode.cyfra.dsl.archive.Value
import io.computenode.cyfra.dsl.archive.binding.GBuffer
import io.computenode.cyfra.dsl.archive.macros.Source

// todo temporary
class GArray2D[T <: Value: {Tag, FromExpr}](width: Int, val arr: GBuffer[T]):
  def at(x: Int32, y: Int32)(using Source): T =
    arr.read(y * width + x)
