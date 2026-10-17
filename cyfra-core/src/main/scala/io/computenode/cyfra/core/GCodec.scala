// scala
package io.computenode.cyfra.core

import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.given
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.types.given
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

  given GCodec[Float16, Float]:
    def toByteBuffer(inBuf: ByteBuffer, values: Array[Float]): ByteBuffer =
      inBuf.clear().order(ByteOrder.nativeOrder())
      values.foreach(value => inBuf.putShort(java.lang.Float.floatToFloat16(value)))
      inBuf.flip()
    def fromByteBuffer(outBuf: ByteBuffer, values: Array[Float]): Array[Float] =
      outBuf.order(ByteOrder.nativeOrder())
      values.indices.foreach(i => values(i) = java.lang.Float.float16ToFloat(outBuf.getShort()))
      outBuf.rewind()
      values

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

  given GCodec[Vec4[Float32], (Float, Float, Float, Float)]:
    def toByteBuffer(inBuf: ByteBuffer, arr: Array[(Float, Float, Float, Float)]): ByteBuffer =
      inBuf.clear().order(ByteOrder.nativeOrder())
      arr
        .flatMap(_.productIterator)
        .map(_.asInstanceOf[Float])
        .foreach: f =>
          inBuf.putFloat(f)
      inBuf.flip()
      inBuf

    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[(Float, Float, Float, Float)]): Array[(Float, Float, Float, Float)] =
      val res = outBuf.asFloatBuffer()
      for i <- 0 until arr.size do arr(i) = (res.get(), res.get(), res.get(), res.get())
      outBuf.rewind()
      arr

  given [T <: Product: {Value, ClassTag}]: GCodec[T, T] with
    def toByteBuffer(inBuf: ByteBuffer, arr: Array[T]): ByteBuffer =
      inBuf.clear()
      arr.foreach(x => inBuf.put(Expression.constantToByteBuffer(x)))
      inBuf.flip()

    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[T]): Array[T] = ???
