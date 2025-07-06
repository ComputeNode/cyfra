package io.computenode.cyfra.dsl.collections

import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.collections.GArray.GArrayElem
import io.computenode.cyfra.dsl.macros.Source
import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

case class GArray[T <: Value: {Tag, FromExpr}](index: Int):
  def at(i: Int32)(using Source): T =
    summon[FromExpr[T]].fromExpr(GArrayElem(index, i.tree))

object GArray:
  case class GArrayElem[T <: Value: Tag](index: Int, i: Expression[Int32]) extends Expression[T]
