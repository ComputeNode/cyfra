package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.Algebra.{*, given}
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.{GArray, GArrayElem}
import izumi.reflect.Tag

case class GArray[T <: Value : Tag : FromExpr](index: Int) {
  def at(i: Int32)(using sourcecode.Name): T = {
    summon[FromExpr[T]].fromExpr(GArrayElem(index, i.tree))
  }
}

class GArray2D[T <: Value : Tag: FromExpr](width: Int, height: Int, val arr: GArray[T]) {
  def at(x: Int32, y: Int32)(using sourcecode.Name): T = {
    arr.at(y * width + x)
  }
}

case class GArrayElem[T <: Value : Tag](index: Int, i: Expression[Int32]) extends Expression[T]

