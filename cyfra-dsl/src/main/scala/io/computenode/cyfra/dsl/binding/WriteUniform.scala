package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import izumi.reflect.Tag

case class WriteUniform[T <: GStruct[T]: Tag](uniform: GUniform[T], value: T) extends GIO[Unit]:
  override def underlying: Unit = ()

