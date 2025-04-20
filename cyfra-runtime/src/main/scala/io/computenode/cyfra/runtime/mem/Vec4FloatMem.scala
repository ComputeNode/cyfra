package io.computenode.cyfra.runtime.mem

import io.computenode.cyfra.dsl.Value.{Float32, Vec4}
import io.computenode.cyfra.runtime.mem.GMem.fRGBA

import java.nio.ByteBuffer

class Vec4FloatMem(val size: Int) extends WritableGMem[Vec4[Float32], fRGBA]:
  def stride: Int = 16

  override protected def toResultArray(buffer: ByteBuffer): Array[fRGBA] = {
    val res = buffer.asFloatBuffer()
    val result = new Array[fRGBA](size)
    for (i <- 0 until size)
      result(i) = (res.get(), res.get(), res.get(), res.get())
    result
  }

  def write(vecs: Array[fRGBA]): Unit = {
    data.rewind()
    vecs.foreach { case (x, y, z, a) =>
      data.putFloat(x)
      data.putFloat(y)
      data.putFloat(z)
      data.putFloat(a)
    }
    data.rewind()
  }

object Vec4FloatMem:
  def apply(vecs: Array[fRGBA]): Vec4FloatMem = {
    val vec4FloatMem = new Vec4FloatMem(vecs.length)
    vec4FloatMem.write(vecs)
    vec4FloatMem
  }

  def apply(size: Int): Vec4FloatMem =
    new Vec4FloatMem(size)
