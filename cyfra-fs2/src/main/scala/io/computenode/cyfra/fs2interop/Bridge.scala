// scala
package io.computenode.cyfra.fs2interop

import io.computenode.cyfra.core.archive.mem.GMem.fRGBA
import io.computenode.cyfra.dsl.*
import fs2.*

import java.nio.{ByteBuffer, ByteOrder}
import izumi.reflect.Tag
import scala.reflect.ClassTag

trait Bridge[CyfraType <: Value: FromExpr: Tag, ScalaType: ClassTag]:
  def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[ScalaType]): ByteBuffer
  def fromByteBuffer(outBuf: ByteBuffer, arr: Array[ScalaType]): Array[ScalaType]

object Bridge:
  given Bridge[Int32, Int]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[Int]): ByteBuffer =
      inBuf.clear().order(ByteOrder.nativeOrder())
      val ib = inBuf.asIntBuffer()
      ib.put(chunk.toArray[Int])
      inBuf.position(ib.position() * java.lang.Integer.BYTES).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Int]): Array[Int] =
      outBuf.order(ByteOrder.nativeOrder())
      outBuf.asIntBuffer().get(arr)
      outBuf.rewind()
      arr

  given Bridge[Float32, Float]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[Float]): ByteBuffer =
      inBuf.clear().order(ByteOrder.nativeOrder())
      val fb = inBuf.asFloatBuffer()
      fb.put(chunk.toArray[Float])
      inBuf.position(fb.position() * java.lang.Float.BYTES).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Float]): Array[Float] =
      outBuf.order(ByteOrder.nativeOrder())
      outBuf.asFloatBuffer().get(arr)
      outBuf.rewind()
      arr

  given Bridge[Vec4[Float32], fRGBA]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[fRGBA]): ByteBuffer =
      inBuf.clear().order(ByteOrder.nativeOrder())
      val vecs = chunk.toArray[fRGBA]
      vecs.foreach:
        case (x, y, z, a) =>
          inBuf.putFloat(x)
          inBuf.putFloat(y)
          inBuf.putFloat(z)
          inBuf.putFloat(a)
      inBuf.flip()
      inBuf

    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[fRGBA]): Array[fRGBA] =
      val res = outBuf.asFloatBuffer()
      for i <- 0 until arr.size do arr(i) = (res.get(), res.get(), res.get(), res.get())
      outBuf.rewind()
      arr

  given Bridge[GBoolean, Boolean]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[Boolean]): ByteBuffer =
      inBuf.put(chunk.toArray.asInstanceOf[Array[Byte]]).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Boolean]): Array[Boolean] =
      outBuf.get(arr.asInstanceOf[Array[Byte]]).flip()
      arr