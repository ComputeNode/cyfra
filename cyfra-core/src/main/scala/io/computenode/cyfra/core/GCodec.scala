// scala
package io.computenode.cyfra.core

import io.computenode.cyfra.dsl.*
import io.computenode.cyfra.dsl.macros.Source
import io.computenode.cyfra.dsl.struct.GStruct.ComposeStruct
import io.computenode.cyfra.dsl.struct.{GStruct, GStructConstructor, GStructSchema}
import io.computenode.cyfra.spirv.SpirvTypes.typeStride
import izumi.reflect.Tag

import java.nio.{ByteBuffer, ByteOrder}
import scala.reflect.ClassTag

trait GCodec[CyfraType <: Value: {FromExpr, Tag}, ScalaType: ClassTag]:
  def toByteBuffer(inBuf: ByteBuffer, arr: Array[ScalaType]): ByteBuffer
  def fromByteBuffer(outBuf: ByteBuffer, arr: Array[ScalaType]): Array[ScalaType]
  def fromByteBufferUnchecked(outBuf: ByteBuffer, arr: Array[Any]): Array[ScalaType] =
    fromByteBuffer(outBuf, arr.asInstanceOf[Array[ScalaType]])

object GCodec:

  def totalStride(gs: GStructSchema[?]): Int = gs.fields
    .map:
      case (_, fromExpr, t) if t <:< gs.gStructTag =>
        val constructor = fromExpr.asInstanceOf[GStructConstructor[?]]
        totalStride(constructor.schema)
      case (_, _, t) =>
        typeStride(t)
    .sum

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
      arr.foreach: tuple =>
        writePrimitive(inBuf, tuple)
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

  given [T <: GStruct[T]: {GStructSchema as schema, Tag, ClassTag}]: GCodec[T, T] with
    def toByteBuffer(inBuf: ByteBuffer, arr: Array[T]): ByteBuffer =
      inBuf.clear().order(ByteOrder.nativeOrder())
      for
        struct <- arr
        field <- struct.productIterator
      do writeConstPrimitive(inBuf, field.asInstanceOf[Value])
      inBuf.flip()
      inBuf
    def fromByteBuffer(outBuf: ByteBuffer, arr: Array[T]): Array[T] =
      val stride = totalStride(schema)
      val nElems = outBuf.remaining() / stride
      for _ <- 0 to nElems do
        val values = schema.fields.map[Value] { case (_, fromExpr, t) =>
          t match
            case t if t <:< schema.gStructTag =>
              val constructor = fromExpr.asInstanceOf[GStructConstructor[T]]
              val nestedValues = constructor.schema.fields.map { case (_, _, nt) =>
                readPrimitive(outBuf, nt)
              }
              constructor.fromExpr(ComposeStruct(nestedValues, constructor.schema))
            case _ =>
              readPrimitive(outBuf, t)
        }
        val newStruct = schema.create(values, schema.copy(dependsOn = None))(using Source("Input"))
        arr.appended(newStruct)
      outBuf.rewind()
      arr

  private def readPrimitive(buffer: ByteBuffer, value: Tag[?]): Value =
    value.tag match
      case t if t =:= summon[Tag[Int]].tag                          => Int32(ConstInt32(buffer.getInt()))
      case t if t =:= summon[Tag[Float]].tag                        => Float32(ConstFloat32(buffer.getFloat()))
      case t if t =:= summon[Tag[Boolean]].tag                      => GBoolean(ConstGB(buffer.get() != 0))
      case t if t =:= summon[Tag[(Float, Float, Float, Float)]].tag => // todo other tuples
        Vec4(
          ComposeVec4(
            Float32(ConstFloat32(buffer.getFloat())),
            Float32(ConstFloat32(buffer.getFloat())),
            Float32(ConstFloat32(buffer.getFloat())),
            Float32(ConstFloat32(buffer.getFloat())),
          ),
        )
      case illegal =>
        throw new IllegalArgumentException(s"Unable to deserialize value of type $illegal")

  private def writeConstPrimitive(buff: ByteBuffer, value: Value): Unit = value.tree match
    case c: Const[?]            => writePrimitive(buff, c.value)
    case compose: ComposeVec[?] =>
      compose.productIterator.foreach: v =>
        writeConstPrimitive(buff, v.asInstanceOf[Value])
    case illegal =>
      throw new IllegalArgumentException(s"Only constant Cyfra values can be serialized (got $illegal)")

  private def writePrimitive(buff: ByteBuffer, value: Any): Unit = value match
    case i: Int     => buff.putInt(i)
    case f: Float   => buff.putFloat(f)
    case b: Boolean => buff.put(if b then 1.toByte else 0.toByte)
    case t: Tuple   =>
      t.productIterator.foreach(writePrimitive(buff, _))
    case illegal =>
      throw new IllegalArgumentException(s"Unable to serialize value $illegal of type ${illegal.getClass}")
