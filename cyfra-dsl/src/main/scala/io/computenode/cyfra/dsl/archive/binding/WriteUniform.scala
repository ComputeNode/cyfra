package io.computenode.cyfra.dsl.archive.binding

import io.computenode.cyfra.dsl.archive.Value
import io.computenode.cyfra.dsl.archive.gio.GIO
import io.computenode.cyfra.dsl.archive.struct.GStruct.Empty
import io.computenode.cyfra.dsl.archive.struct.{GStruct, GStructSchema}
import izumi.reflect.Tag

case class WriteUniform[T <: GStruct[?]: {Tag, GStructSchema}](uniform: GUniform[T], value: T) extends GIO[Empty]:
  override def underlying: Empty = Empty()
