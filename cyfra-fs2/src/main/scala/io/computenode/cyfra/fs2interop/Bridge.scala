package io.computenode.cyfra.fs2interop

import io.computenode.cyfra.core.aalegacy.mem.GMem.fRGBA
import io.computenode.cyfra.dsl.*

import fs2.*
import java.nio.ByteBuffer
import org.lwjgl.BufferUtils
import izumi.reflect.Tag

import scala.reflect.ClassTag

trait Bridge[CyfraType <: Value: FromExpr: Tag, ScalaType: ClassTag]:
  def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[ScalaType]): ByteBuffer
  def fromByteBuffer(outBuf: ByteBuffer, arr: Array[ScalaType]): Array[ScalaType]

object Bridge:
  given Bridge[Int32, Int]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[Int]): ByteBuffer =
      inBuf.asIntBuffer().put(chunk.toArray[Int]).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Int]): Array[Int] =
      outBuf.asIntBuffer().get(arr).flip()
      arr

  given Bridge[Float32, Float]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[Float]): ByteBuffer =
      inBuf.asFloatBuffer().put(chunk.toArray[Float]).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Float]): Array[Float] =
      outBuf.asFloatBuffer().get(arr).flip()
      arr

  given Bridge[Vec4[Float32], fRGBA]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[fRGBA]): ByteBuffer =
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
      outBuf.flip()
      arr

  given Bridge[GBoolean, Boolean]:
    def toByteBuffer(inBuf: ByteBuffer, chunk: Chunk[Boolean]): ByteBuffer =
      inBuf.put(chunk.toArray.asInstanceOf[Array[Byte]]).flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[Boolean]): Array[Boolean] =
      outBuf.get(arr.asInstanceOf[Array[Byte]]).flip()
      arr
