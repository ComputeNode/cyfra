package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.dsl.{Expression, Value}
import izumi.reflect.Tag

case class ReadUniform[T <: GStruct[?]: {Tag, GStructSchema}](uniform: GUniform[T]) extends Expression[T]
