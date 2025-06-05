package io.computenode.cyfra.dsl

import io.computenode.cyfra.dsl.GStruct.Empty
import izumi.reflect.Tag

class UniformContext[G <: GStruct[G] : Tag: GStructSchema](val uniform: G)
object UniformContext:
  def withUniform[G <: GStruct[G] : Tag: GStructSchema, T](uniform: G)(fn: UniformContext[G] ?=> T): T =
    fn(using UniformContext(uniform))
  given empty: UniformContext[Empty] = new UniformContext(Empty())