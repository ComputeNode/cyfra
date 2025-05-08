package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Value

import java.nio.ByteBuffer

trait RamGMem[T <: Value, R] extends GMem[T]:
  protected val data: ByteBuffer
  def toReadOnlyBuffer: ByteBuffer = data.asReadOnlyBuffer()
  def write(data: Array[R]): Unit
