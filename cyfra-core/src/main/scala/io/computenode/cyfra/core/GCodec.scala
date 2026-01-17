// scala
package io.computenode.cyfra.core

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import izumi.reflect.Tag

import java.nio.{ByteBuffer, ByteOrder}
import scala.reflect.ClassTag

trait GCodec[CyfraType: Value, ScalaType: ClassTag]:
  def toByteBuffer(inBuf: ByteBuffer, arr: Array[ScalaType]): ByteBuffer
  def fromByteBuffer(outBuf: ByteBuffer, arr: Array[ScalaType]): Array[ScalaType]
  def fromByteBufferUnchecked(outBuf: ByteBuffer, arr: Array[Any]): Array[ScalaType] =
    fromByteBuffer(outBuf, arr.asInstanceOf[Array[ScalaType]])

object GCodec:

  def toByteBuffer[CyfraType: Value, ScalaType: ClassTag](inBuf: ByteBuffer, arr: Array[ScalaType])(using
    gc: GCodec[CyfraType, ScalaType],
  ): ByteBuffer =
    gc.toByteBuffer(inBuf, arr)

  def fromByteBuffer[CyfraType: Value, ScalaType: ClassTag](outBuf: ByteBuffer, arr: Array[ScalaType])(using
    gc: GCodec[CyfraType, ScalaType],
  ): Array[ScalaType] =
    gc.fromByteBuffer(outBuf, arr)

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

  given GCodec[Bool, Boolean]:
    def toByteBuffer(inBuf: ByteBuffer, arr: Array[Boolean]): ByteBuffer =
      inBuf.put(arr.asInstanceOf[Array[Byte]]).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Boolean]): Array[Boolean] =
      outBuf.get(arr.asInstanceOf[Array[Byte]]).flip()
      arr