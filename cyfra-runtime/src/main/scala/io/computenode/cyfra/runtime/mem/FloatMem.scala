package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Value.Float32

import java.nio.ByteBuffer

class FloatMem(val size: Int) extends WritableGMem[Float32, Float]:

  def stride: Int = 4

  override protected def toResultArray(buffer: ByteBuffer): Array[Float] = {
    val res = buffer.asFloatBuffer()
    val result = new Array[Float](size)
    res.get(result)
    result
  }

  def write(floats: Array[Float]): Unit = {
    data.rewind()
    data.asFloatBuffer().put(floats)
    data.rewind()
  }

object FloatMem {
  def apply(floats: Array[Float]): FloatMem = {
    val floatMem = new FloatMem(floats.length)
    floatMem.write(floats)
    floatMem
  }

  def apply(size: Int): FloatMem =
    new FloatMem(size)
}
