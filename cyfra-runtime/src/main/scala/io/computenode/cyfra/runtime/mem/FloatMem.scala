package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Value.Float32
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer
import org.lwjgl.system.MemoryUtil

class FloatMem(val size: Int, protected val data: ByteBuffer) extends RamGMem[Float32, Float]:
  def toArray: Array[Float] =
    val res = data.asFloatBuffer()
    val result = new Array[Float](size)
    res.get(result)
    result

object FloatMem:
  val FloatSize = 4

  def apply(floats: Array[Float]): FloatMem =
    val size = floats.length
    val data = BufferUtils.createByteBuffer(size * FloatSize)
    data.asFloatBuffer().put(floats)
    data.rewind()
    new FloatMem(size, data)

  def apply(size: Int): FloatMem =
    val data = BufferUtils.createByteBuffer(size * FloatSize)
    new FloatMem(size, data)
