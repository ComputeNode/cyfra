package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.Expression
import io.computenode.cyfra.dsl.struct.GStruct
import izumi.reflect.Tag

case class ReadUniform[T <: GStruct[T] : Tag](uniform: GUniform[T]) extends Expression[T]