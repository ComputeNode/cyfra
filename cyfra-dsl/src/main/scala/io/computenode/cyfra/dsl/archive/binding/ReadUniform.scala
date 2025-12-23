package io.computenode.cyfra.dsl.archive.binding

import io.computenode.cyfra.dsl.archive.{Expression, Value}
import io.computenode.cyfra.dsl.archive.struct.{GStruct, GStructSchema}
import izumi.reflect.Tag

case class ReadUniform[T <: GStruct[?]: {Tag, GStructSchema}](uniform: GUniform[T]) extends Expression[T]
