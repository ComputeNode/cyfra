package io.computenode.cyfra.dsl.control

import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

case class Scope[T <: Value: Tag](expr: Expression[T], isDetached: Boolean = false):
  def rootTreeId: Int = expr.treeid