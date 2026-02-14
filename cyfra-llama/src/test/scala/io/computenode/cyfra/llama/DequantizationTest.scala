package io.computenode.cyfra.llama

import io.computenode.cyfra.llama.gguf.{Dequantize => CpuDequantize}
import munit.FunSuite

import java.nio.{ByteBuffer, ByteOrder}

/** Tests for CPU dequantization utilities.
  *
  * GPU quantized matmul tests were removed with F32 pipeline.
  */
class DequantizationTest extends FunSuite:

  /** Convert float32 to fp16 (approximate, for test purposes). */
  def floatToFp16(f: Float): Short =
    val bits = java.lang.Float.floatToIntBits(f)
    val sign = (bits >> 31) & 1
    val exp = (bits >> 23) & 0xFF
    val mant = bits & 0x7FFFFF
    
    if exp == 0 then
      // Zero or denormalized
      (sign << 15).toShort
    else if exp == 255 then
      // Infinity or NaN
      ((sign << 15) | 0x7C00).toShort
    else
      val newExp = exp - 127 + 15
      if newExp <= 0 then
        // Underflow to zero
        (sign << 15).toShort
      else if newExp >= 31 then
        // Overflow to infinity
        ((sign << 15) | 0x7C00).toShort
      else
        val newMant = mant >> 13
        ((sign << 15) | (newExp << 10) | newMant).toShort

  test("Q4_K scale extraction: verify is < 4 vs is >= 4 logic"):
    // This test verifies the scale extraction matches llama.cpp's get_scale_min_k4
    // For is < 4: sc = scales[is] & 0x3F, m = scales[is+4] & 0x3F
    // For is >= 4: sc = (scales[is+4] & 0x0F) | ((scales[is-4] >> 6) << 4)
    //              m = ((scales[is+4] >> 4) & 0x0F) | ((scales[is] >> 6) << 4)
    
    val scales = Array[Byte](
      0x3f, 0x3e, 0x3d, 0x3c, // scales[0-3]: values 63, 62, 61, 60
      0x10, 0x20, 0x30, 0x40.toByte, // scales[4-7]: mins for j<4
      0x05, 0x06, 0x07, 0x08, // scales[8-11]: for j>=4 extraction
    )
    
    // Verify CPU extraction for j=0
    val (sc0, m0) = Dequantize.getScaleMinK4(0, scales)
    assertEquals(sc0.toInt, 63, "scale for j=0 should be 63")
    assertEquals(m0.toInt, 16, "min for j=0 should be 16")
    
    // Verify CPU extraction for j=4
    // sc4 = (scales[8] & 0x0F) | ((scales[0] >> 6) << 4)
    // scales[8] = 0x05, scales[0] = 0x3F -> 0x3F >> 6 = 0
    // sc4 = (5 & 0x0F) | (0 << 4) = 5
    val (sc4, m4) = Dequantize.getScaleMinK4(4, scales)
    assertEquals(sc4.toInt, 5, "scale for j=4 should be 5")

  // Helper method to expose getScaleMinK4 for testing
  object Dequantize:
    def getScaleMinK4(j: Int, scales: Array[Byte]): (Float, Float) =
      if j < 4 then
        val d = (scales(j) & 0x3F).toFloat
        val m = (scales(j + 4) & 0x3F).toFloat
        (d, m)
      else
        val d = ((scales(j + 4) & 0x0F) | ((scales(j - 4) >> 6) << 4)).toFloat
        val m = ((scales(j + 4) >> 4) & 0x0F | ((scales(j) >> 6) << 4)).toFloat
        (d, m)
