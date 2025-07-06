package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.Value.FromExpr.fromExpr
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import izumi.reflect.Tag

trait GUniform[T <: Value: Tag: FromExpr] extends GBinding[T]:
  def read: T = fromExpr(ReadUniform(this))

  def write(value: T): GIO[Unit] = WriteUniform(this, value)

object GUniform:

  case class ParamUniform[T <: GStruct[T]: Tag: FromExpr]() extends GUniform[T]

  def fromParams[T <: GStruct[T]: Tag: FromExpr] = ParamUniform[T]()
