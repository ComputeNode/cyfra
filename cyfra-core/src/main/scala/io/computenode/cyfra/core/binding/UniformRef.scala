package io.computenode.cyfra.core.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

case class UniformRef[T <: GStruct[?]: {Tag, FromExpr, GStructSchema}](layoutOffset: Int) extends GUniform[T]
