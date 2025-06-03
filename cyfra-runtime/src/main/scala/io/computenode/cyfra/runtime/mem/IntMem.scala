package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Value.Int32
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer
import org.lwjgl.system.MemoryUtil

class IntMem(val size: Int, protected val data: ByteBuffer) extends RamGMem[Int32, Int]:
  def toArray: Array[Int] =
    val res = data.asIntBuffer()
    val result = new Array[Int](size)
    res.get(result)
    result


object IntMem:
  val IntSize = 4

  def apply(ints: Array[Int]): IntMem =
    val size = ints.length
    val data = BufferUtils.createByteBuffer(size * IntSize)
    data.asIntBuffer().put(ints)
    data.rewind()
    new IntMem(size, data)

  def apply(size: Int): IntMem = 
    val data = BufferUtils.createByteBuffer(size * IntSize)
    new IntMem(size, data)
