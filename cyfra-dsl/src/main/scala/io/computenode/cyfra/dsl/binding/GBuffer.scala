package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.{FromExpr, Int32}
import io.computenode.cyfra.dsl.gio.GIO
import izumi.reflect.Tag

trait GBuffer[T <: Value: FromExpr: Tag] extends GBinding:
  def read(index: Int32): T = FromExpr.fromExpr(ReadBuffer(this, index))
  
  def write(index: Int32, value: T): GIO[Unit] = GIO.write(this, index, value)

object GBuffer