package io.computenode.cyfra.dsl.archive.collections

import io.computenode.cyfra.dsl.archive.Value.*
import io.computenode.cyfra.dsl.archive.binding.{GBuffer, ReadBuffer}
import io.computenode.cyfra.dsl.archive.macros.Source
import io.computenode.cyfra.dsl.archive.{Expression, Value}
import izumi.reflect.Tag

// todo temporary
case class GArray[T <: Value: {Tag, FromExpr}](underlying: GBuffer[T]):
  def at(i: Int32)(using Source): T =
    summon[FromExpr[T]].fromExpr(ReadBuffer(underlying, i))
