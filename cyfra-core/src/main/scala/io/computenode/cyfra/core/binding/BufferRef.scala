package io.computenode.cyfra.core.binding

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.binding.{GBuffer, NoProvenance, Provenance as ProvenanceBase}
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

case class BufferRef[T <: Value: {Tag, FromExpr}](
  layoutOffset: Int,
  provenance: ProvenanceBase = NoProvenance,
) extends GBuffer[T]:
  override def withProvenance(p: ProvenanceBase): GBuffer[T] = copy(provenance = p)
