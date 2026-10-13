package io.computenode.cyfra.core

import io.computenode.cyfra.core.expression.Expression
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

import java.nio.{ByteBuffer, ByteOrder}

class GCodecTest extends munit.FunSuite:
  case class Mixed(a: Int16, b: Float64, c: Float32, d: Float16)

  test("constants are encoded by their declared type"):
    val bytes = Expression.constantToByteBuffer(Mixed(Int16(3), Float64(2.5), 1.5f, Float16(0.5f)))
    val bb = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    assertEquals(bytes.length, 2 + 8 + 4 + 2)
    assertEquals((bb.getShort(), bb.getDouble(), bb.getFloat(), java.lang.Float.float16ToFloat(bb.getShort())), (3.toShort, 2.5, 1.5f, 0.5f))

  test("Bool constants have no buffer representation"):
    intercept[IllegalArgumentException](Expression.constantToByteBuffer(Bool(true)))

  case class Params(n: Int32, scale: Float32)

  test("struct codec encodes mixed fields and leaves the buffer ready to read"):
    val bb = ByteBuffer.allocate(16).order(ByteOrder.nativeOrder())
    summon[GCodec[Params, Params]].toByteBuffer(bb, Array(Params(7, 2.5f), Params(-1, 0.5f)))
    assertEquals(bb.remaining(), 16)
    assertEquals((bb.getInt(), bb.getFloat(), bb.getInt(), bb.getFloat()), (7, 2.5f, -1, 0.5f))
