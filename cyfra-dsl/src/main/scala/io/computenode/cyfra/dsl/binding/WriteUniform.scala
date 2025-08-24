package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

case class WriteUniform[T <: GStruct[?]: {Tag, GStructSchema}](uniform: GUniform[T], value: T) extends GIO[Empty]:
  override def underlying: Empty = Empty()
