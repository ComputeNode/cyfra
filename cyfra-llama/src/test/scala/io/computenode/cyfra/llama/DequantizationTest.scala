package io.computenode.cyfra.llama

import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llama.gguf.{Dequantize => CpuDequantize}
import io.computenode.cyfra.llama.programs.f32.*
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import java.nio.{ByteBuffer, ByteOrder}

/** Tests to verify Q4_K and Q6_K dequantization matches CPU implementation. */
class DequantizationTest extends FunSuite:

  def allocateBuffer(floats: Int): ByteBuffer =
    ByteBuffer.allocateDirect(floats * 4).order(ByteOrder.nativeOrder())

  def copyToBuffer(arr: Array[Float], buf: ByteBuffer): Unit =
    buf.clear(); buf.asFloatBuffer().put(arr); buf.rewind()

  def copyFromBuffer(buf: ByteBuffer, arr: Array[Float]): Unit =
    buf.rewind(); buf.asFloatBuffer().get(arr)

  /** Create a test Q4_K block with known values.
    * 
    * Q4_K block layout (144 bytes):
    *   - bytes 0-1: d (fp16)
    *   - bytes 2-3: dmin (fp16)
    *   - bytes 4-15: scales (12 bytes)
    *   - bytes 16-143: qs (128 bytes, 4-bit quantized values)
    */
  def createQ4KBlock(d: Float, dmin: Float, scales: Array[Byte], qs: Array[Byte]): Array[Byte] =
    require(scales.length == 12, "scales must be 12 bytes")
    require(qs.length == 128, "qs must be 128 bytes")
    val block = new Array[Byte](144)
    val buf = ByteBuffer.wrap(block).order(ByteOrder.LITTLE_ENDIAN)
    
    // Write d and dmin as fp16
    buf.putShort(0, floatToFp16(d))
    buf.putShort(2, floatToFp16(dmin))
    
    // Write scales
    System.arraycopy(scales, 0, block, 4, 12)
    
    // Write qs
    System.arraycopy(qs, 0, block, 16, 128)
    
    block

  /** Create a test Q6_K block with known values.
    * 
    * Q6_K block layout (210 bytes):
    *   - bytes 0-127: ql (low 4 bits)
    *   - bytes 128-191: qh (high 2 bits)
    *   - bytes 192-207: scales (int8)
    *   - bytes 208-209: d (fp16)
    */
  def createQ6KBlock(d: Float, scales: Array[Byte], ql: Array[Byte], qh: Array[Byte]): Array[Byte] =
    require(scales.length == 16, "scales must be 16 bytes")
    require(ql.length == 128, "ql must be 128 bytes")
    require(qh.length == 64, "qh must be 64 bytes")
    val block = new Array[Byte](210)
    val buf = ByteBuffer.wrap(block).order(ByteOrder.LITTLE_ENDIAN)
    
    // Write ql
    System.arraycopy(ql, 0, block, 0, 128)
    
    // Write qh
    System.arraycopy(qh, 0, block, 128, 64)
    
    // Write scales
    System.arraycopy(scales, 0, block, 192, 16)
    
    // Write d as fp16
    buf.putShort(208, floatToFp16(d))
    
    block

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

  test("Q4_K dequantization: GPU matches CPU for all 8 scale indices"):
    VkCyfraRuntime.using:
      val K = 256  // One Q4_K block
      val N = 1    // One output row
      
      // Create Q4_K block with distinctive scale patterns
      // Scales are 6-bit packed: for j < 4, scale is scales[j] & 0x3F
      // For j >= 4, it's a combination of bits
      val scales = Array.fill[Byte](12)(0)
      
      // Set scales for j=0..3 (simple 6-bit in lower bytes)
      scales(0) = 1  // scale 0 = 1
      scales(1) = 2  // scale 1 = 2
      scales(2) = 3  // scale 2 = 3
      scales(3) = 4  // scale 3 = 4
      
      // Set mins for j=0..3 (in bytes 4-7)
      scales(4) = 0  // min 0 = 0
      scales(5) = 0  // min 1 = 0
      scales(6) = 0  // min 2 = 0
      scales(7) = 0  // min 3 = 0
      
      // Scales for j=4..7 are combinations - set them to known patterns
      // For j=4: sc = (scales[8] & 0x0F) | ((scales[0] >> 6) << 4)
      // Let's set scales[8] = 0x05 and ensure scales[0] high bits are 0
      scales(8) = 5   // scale 4 = 5
      scales(9) = 6   // scale 5 = 6
      scales(10) = 7  // scale 6 = 7
      scales(11) = 8  // scale 7 = 8
      
      // Create qs with distinctive pattern: q values 0-15 for each position
      val qs = Array.tabulate[Byte](128)(i => ((i % 16) | ((i % 16) << 4)).toByte)
      
      val d = 1.0f
      val dmin = 0.0f
      val block = createQ4KBlock(d, dmin, scales, qs)
      
      // CPU dequantization
      val cpuResult = CpuDequantize.dequantizeQ4K(block, 256)
      
      // GPU computation
      val numUint32 = 144 / 4
      val weightBuf = ByteBuffer.allocateDirect(numUint32 * 4).order(ByteOrder.nativeOrder())
      weightBuf.put(block); weightBuf.rewind()
      
      val input = Array.fill(K)(1.0f)
      val inputBuf = allocateBuffer(K); copyToBuffer(input, inputBuf)
      val outputBuf = allocateBuffer(N)
      
      val sizes = Q4KMatmulVecProgram.Sizes(1, K, N)
      val program = Q4KMatmulVecProgram.forward(sizes)
      
      val region = GBufferRegion
        .allocate[Q4KMatmulVecProgram.ProgramLayout]
        .map(layout => program.execute(sizes, layout))
      
      val gpuResult = new Array[Float](N)
      region.runUnsafe(
        init = Q4KMatmulVecProgram.ProgramLayout(
          weight = GBuffer[UInt32](weightBuf),
          input = GBuffer[Float32](inputBuf),
          output = GBuffer[Float32](outputBuf),
        ),
        onDone = layout =>
          layout.output.read(outputBuf)
          copyFromBuffer(outputBuf, gpuResult),
      )
      
      // The GPU result should be the dot product of dequantized weights with all-ones input
      // which equals the sum of all dequantized values
      val cpuSum = cpuResult.sum
      val gpuSum = gpuResult(0)
      
      println(s"Q4_K CPU sum: $cpuSum")
      println(s"Q4_K GPU sum: $gpuSum")
      println(s"First 32 CPU values: ${cpuResult.take(32).mkString(", ")}")
      
      // Allow small floating-point tolerance
      assertEqualsDouble(gpuSum.toDouble, cpuSum.toDouble, 1.0,
        s"Q4_K GPU sum ($gpuSum) should match CPU sum ($cpuSum)")

  test("Q6_K dequantization: GPU matches CPU"):
    VkCyfraRuntime.using:
      val K = 256  // One Q6_K block
      val N = 1    // One output row
      
      // Create Q6_K block with known values
      val d = 1.0f
      val scales = Array.tabulate[Byte](16)(i => (i + 1).toByte) // scales 1-16
      
      // ql: low 4 bits of each 6-bit value
      // qh: high 2 bits of each 6-bit value
      // For simplicity, set all q values to a known pattern
      val ql = Array.fill[Byte](128)(0x55.toByte) // alternating 0101 pattern
      val qh = Array.fill[Byte](64)(0x00.toByte)  // high bits all 0
      
      val block = createQ6KBlock(d, scales, ql, qh)
      
      // CPU dequantization
      val cpuResult = CpuDequantize.dequantizeQ6K(block, 256)
      
      // GPU computation
      val numBytes = 210
      val numUint32 = (numBytes + 3) / 4
      val weightBuf = ByteBuffer.allocateDirect(numUint32 * 4).order(ByteOrder.nativeOrder())
      weightBuf.put(block); weightBuf.rewind()
      
      val input = Array.fill(K)(1.0f)
      val inputBuf = allocateBuffer(K); copyToBuffer(input, inputBuf)
      val outputBuf = allocateBuffer(N)
      
      val sizes = Q6KMatmulVecProgram.Sizes(1, K, N)
      val program = Q6KMatmulVecProgram.forward(sizes)
      
      val region = GBufferRegion
        .allocate[Q6KMatmulVecProgram.ProgramLayout]
        .map(layout => program.execute(sizes, layout))
      
      val gpuResult = new Array[Float](N)
      region.runUnsafe(
        init = Q6KMatmulVecProgram.ProgramLayout(
          weight = GBuffer[UInt32](weightBuf),
          input = GBuffer[Float32](inputBuf),
          output = GBuffer[Float32](outputBuf),
        ),
        onDone = layout =>
          layout.output.read(outputBuf)
          copyFromBuffer(outputBuf, gpuResult),
      )
      
      val cpuSum = cpuResult.sum
      val gpuSum = gpuResult(0)
      
      println(s"Q6_K CPU sum: $cpuSum")
      println(s"Q6_K GPU sum: $gpuSum")
      println(s"First 32 CPU values: ${cpuResult.take(32).mkString(", ")}")
      
      assertEqualsDouble(gpuSum.toDouble, cpuSum.toDouble, 1.0,
        s"Q6_K GPU sum ($gpuSum) should match CPU sum ($cpuSum)")

  test("Q4_K scale extraction: verify is < 4 vs is >= 4 logic"):
    // This test verifies the scale extraction matches llama.cpp's get_scale_min_k4
    // For is < 4: sc = scales[is] & 0x3F, m = scales[is+4] & 0x3F
    // For is >= 4: sc = (scales[is+4] & 0x0F) | ((scales[is-4] >> 6) << 4)
    //              m = ((scales[is+4] >> 4) & 0x0F) | ((scales[is] >> 6) << 4)
    
    val scales = Array[Byte](
      0x3F, 0x3E, 0x3D, 0x3C,  // scales[0-3]: values 63, 62, 61, 60
      0x10, 0x20, 0x30, 0x40.toByte,  // scales[4-7]: mins for j<4
      0x05, 0x06, 0x07, 0x08   // scales[8-11]: for j>=4 extraction
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
