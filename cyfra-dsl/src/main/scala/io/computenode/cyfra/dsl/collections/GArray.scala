package io.computenode.cyfra.dsl.collections

import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.binding.{GBuffer, ReadBuffer}
import io.computenode.cyfra.dsl.macros.Source
import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

// todo temporary
case class GArray[T <: Value: {Tag, FromExpr}](underlying: GBuffer[T]):
  def at(i: Int32)(using Source): T =
    summon[FromExpr[T]].fromExpr(ReadBuffer(underlying, i))

