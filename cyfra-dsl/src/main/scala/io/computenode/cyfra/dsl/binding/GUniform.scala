package io.computenode.cyfra.dsl.binding

import io.computenode.cyfra.dsl.struct.GStruct

trait GUniform[T <: GStruct[T]] extends GBinding
  def read: ReadUniform[T]

object GUniform