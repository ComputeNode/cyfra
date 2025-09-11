// scala
package io.computenode.cyfra.core

import io.computenode.cyfra.core.archive.mem.GMem.{fRGBA, totalStride}
import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import izumi.reflect.Tag

import java.nio.{ByteBuffer, ByteOrder}
import scala.reflect.ClassTag

trait GCodec[CyfraType <: Value: {FromExpr, Tag}, ScalaType: ClassTag]:
  def toByteBuffer(inBuf: ByteBuffer, arr: Array[ScalaType]): ByteBuffer
  def fromByteBuffer(outBuf: ByteBuffer, arr: Array[ScalaType]): Array[ScalaType]

object GCodec:
  given GCodec[Int32, Int]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Array[Int]): ByteBuffer =
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

  given GCodec[Float32, Float]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Array[Float]): ByteBuffer =
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

  given GCodec[Vec4[Float32], fRGBA]:
    def toByteBuffer(inBuf: ByteBuffer, arr: Array[fRGBA]): ByteBuffer =
      inBuf.clear().order(ByteOrder.nativeOrder())
      arr.foreach:
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

  given GCodec[GBoolean, Boolean]:
    def toByteBuffer(inBuf: ByteBuffer, arr: Array[Boolean]): ByteBuffer =
      inBuf.put(arr.asInstanceOf[Array[Byte]]).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Boolean]): Array[Boolean] =
      outBuf.get(arr.asInstanceOf[Array[Byte]]).flip()
      arr
