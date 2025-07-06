package io.computenode.cyfra.core.archive

import io.computenode.cyfra.core.archive.UniformContext
import io.computenode.cyfra.dsl.struct.*
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import izumi.reflect.Tag

class UniformContext[G <: GStruct[G]: Tag: GStructSchema](val uniform: G)

object UniformContext:
  def withUniform[G <: GStruct[G]: Tag: GStructSchema, T](uniform: G)(fn: UniformContext[G] ?=> T): T =
    fn(using UniformContext(uniform))
  given empty: UniformContext[Empty] = new UniformContext(Empty())
