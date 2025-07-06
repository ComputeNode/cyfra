package io.computenode.cyfra.core.archive.mem

import io.computenode.cyfra.core.archive.mem.GMem.fRGBA
import io.computenode.cyfra.dsl.Value.{Float32, Vec4}
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer

class Vec4FloatMem(val size: Int, protected val data: ByteBuffer) extends RamGMem[Vec4[Float32], fRGBA]:
  def toArray: Array[fRGBA] =
    val res = data.asFloatBuffer()
    val result = new Array[fRGBA](size)
    for i <- 0 until size do result(i) = (res.get(), res.get(), res.get(), res.get())
    result

object Vec4FloatMem:
  val Vec4FloatSize = 16

  def apply(vecs: Array[fRGBA]): Vec4FloatMem =
    val size = vecs.length
    val data = BufferUtils.createByteBuffer(size * Vec4FloatSize)
    vecs.foreach { case (x, y, z, a) =>
      data.putFloat(x)
      data.putFloat(y)
      data.putFloat(z)
      data.putFloat(a)
    }
    data.rewind()
    new Vec4FloatMem(size, data)

  def apply(size: Int): Vec4FloatMem =
    val data = BufferUtils.createByteBuffer(size * Vec4FloatSize)
    new Vec4FloatMem(size, data)
