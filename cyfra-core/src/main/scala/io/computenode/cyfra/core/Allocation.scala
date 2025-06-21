package io.computenode.cyfra.core

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.dsl.buffer.GBuffer

import java.nio.ByteBuffer

trait Allocation:
  extension [R, T <: Value](buffer: GBuffer[T])
    def read(bb: ByteBuffer): Unit =
      ()

    def write(bb: ByteBuffer): Unit =
      ()
