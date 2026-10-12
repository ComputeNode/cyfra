package io.computenode.cyfra.core

import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

import java.nio.{ByteBuffer, ByteOrder}

class GCodecTest extends munit.FunSuite:
  case class Params(n: Int32, scale: Float32)

  test("struct codec encodes mixed fields and leaves the buffer ready to read"):
    val bb = ByteBuffer.allocate(16).order(ByteOrder.nativeOrder())
    summon[GCodec[Params, Params]].toByteBuffer(bb, Array(Params(7, 2.5f), Params(-1, 0.5f)))
    assertEquals(bb.remaining(), 16)
    assertEquals((bb.getInt(), bb.getFloat(), bb.getInt(), bb.getFloat()), (7, 2.5f, -1, 0.5f))
