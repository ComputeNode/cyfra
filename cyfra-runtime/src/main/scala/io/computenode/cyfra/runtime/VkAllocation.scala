package io.computenode.cyfra.runtime

import io.computenode.cyfra.core.layout.{Layout, LayoutStruct}
import io.computenode.cyfra.core.{Allocation, GExecution}
import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.binding.GBuffer

import java.nio.ByteBuffer

class VkAllocation extends Allocation:
  extension [R, T <: Value](buffer: GBuffer[T])
    override def read(bb: ByteBuffer): Unit = ()

    override def write(bb: ByteBuffer): Unit = ()

  extension [Params, L <: Layout, RL <: Layout : LayoutStruct](execution: GExecution[Params, L, RL])
    override def execute(params: Params, layout: L): RL =
      ???
