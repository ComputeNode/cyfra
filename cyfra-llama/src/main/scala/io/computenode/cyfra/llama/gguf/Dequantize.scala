package io.computenode.cyfra.llama.gguf

import java.nio.{ByteBuffer, ByteOrder}

/** Dequantization functions for GGUF quantized tensors.
  * 
  * Based on llama.cpp's ggml-quants.c
  */
object Dequantize:
  
  val QK_K = 256  // Block size for K-quants

  /** Convert half-precision float16 to float32.
    * 
    * IEEE 754 half-precision: 1 sign bit, 5 exponent bits, 10 mantissa bits
    */
  def fp16ToFp32(h: Short): Float =
    val sign = (h >> 15) & 1
    val exp = (h >> 10) & 0x1F
    val mant = h & 0x3FF
    
    if exp == 0 then
      // Denormalized or zero
      if mant == 0 then
        if sign == 1 then -0.0f else 0.0f
      else
        // Denormalized number
        val f = mant.toFloat / 1024.0f
        val result = f * math.pow(2, -14).toFloat
        if sign == 1 then -result else result
    else if exp == 31 then
      // Infinity or NaN
      if mant == 0 then
        if sign == 1 then Float.NegativeInfinity else Float.PositiveInfinity
      else
        Float.NaN
    else
      // Normalized number
      val f = 1.0f + mant.toFloat / 1024.0f
      val result = f * math.pow(2, exp - 15).toFloat
      if sign == 1 then -result else result

  /** Dequantize Q4_K block to float32.
    * 
    * Q4_K format:
    * - 256 values per block
    * - 2x float16 for d and dmin
    * - 12 bytes for scales (6-bit each, packed)
    * - 128 bytes for quantized values (4-bit each, packed)
    * - Total: 144 bytes per block
    */
  def dequantizeQ4K(data: Array[Byte], numElements: Long): Array[Float] =
    val numBlocks = (numElements / QK_K).toInt
    val result = new Array[Float](numElements.toInt)
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    
    var resultIdx = 0
    for blockIdx <- 0 until numBlocks do
      val blockStart = blockIdx * 144
      
      // Read d and dmin (fp16)
      val dHalf = buf.getShort(blockStart)
      val dminHalf = buf.getShort(blockStart + 2)
      val d = fp16ToFp32(dHalf)
      val dmin = fp16ToFp32(dminHalf)
      
      // Read scales (12 bytes, 6-bit values packed)
      val scales = new Array[Byte](12)
      for i <- 0 until 12 do
        scales(i) = buf.get(blockStart + 4 + i)
      
      // Read quantized values (128 bytes, 4-bit packed)
      val qs = new Array[Byte](128)
      for i <- 0 until 128 do
        qs(i) = buf.get(blockStart + 16 + i)
      
      // Dequantize 256 values in groups of 64
      var is = 0
      var qsIdx = 0
      for j <- 0 until 4 do  // 4 groups of 64
        // Get scale and min for this group (two sub-groups of 32)
        val (sc1, m1) = getScaleMinK4(is, scales)
        val (sc2, m2) = getScaleMinK4(is + 1, scales)
        
        val d1 = d * sc1
        val m1Val = dmin * m1
        val d2 = d * sc2
        val m2Val = dmin * m2
        
        // First 32 values (low nibble)
        for l <- 0 until 32 do
          val q = qs(qsIdx + l) & 0x0F
          result(resultIdx) = d1 * q - m1Val
          resultIdx += 1
        
        // Second 32 values (high nibble)
        for l <- 0 until 32 do
          val q = (qs(qsIdx + l) >> 4) & 0x0F
          result(resultIdx) = d2 * q - m2Val
          resultIdx += 1
        
        qsIdx += 32
        is += 2
    
    result

  /** Get scale and min from packed 6-bit values in Q4_K scales array.
    * 
    * Matches llama.cpp's get_scale_min_k4 implementation exactly.
    * scales array is 12 bytes, j ranges 0-7.
    * 
    * IMPORTANT: Use & 0xFF to convert signed bytes to unsigned before shifting,
    * otherwise Java's signed byte extension causes incorrect results when bit 7 is set.
    */
  private def getScaleMinK4(j: Int, scales: Array[Byte]): (Float, Float) =
    if j < 4 then
      // Simple 6-bit extraction from lower bytes
      val d = (scales(j) & 0x3F).toFloat
      val m = (scales(j + 4) & 0x3F).toFloat
      (d, m)
    else
      // Combine bits from different positions - use & 0xFF for unsigned interpretation
      val sj4 = scales(j + 4) & 0xFF  // scales[j+4] as unsigned
      val sjm4 = scales(j - 4) & 0xFF // scales[j-4] as unsigned  
      val sj = scales(j) & 0xFF       // scales[j] as unsigned
      val d = ((sj4 & 0x0F) | ((sjm4 >> 6) << 4)).toFloat
      val m = (((sj4 >> 4) & 0x0F) | ((sj >> 6) << 4)).toFloat
      (d, m)

  /** Dequantize Q6_K block to float32.
    * 
    * Q6_K format (matches llama.cpp exactly):
    * - 256 values per block
    * - 128 bytes for low 4 bits (ql)
    * - 64 bytes for high 2 bits (qh)
    * - 16 bytes for scales (int8)
    * - 2 bytes for d (fp16)
    * - Total: 210 bytes per block
    */
  def dequantizeQ6K(data: Array[Byte], numElements: Long): Array[Float] =
    val numBlocks = (numElements / QK_K).toInt
    val result = new Array[Float](numElements.toInt)
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    
    for blockIdx <- 0 until numBlocks do
      val blockStart = blockIdx * 210
      val blockResultStart = blockIdx * QK_K
      
      // Read d (fp16) at offset 208
      val d = fp16ToFp32(buf.getShort(blockStart + 208))
      
      // Two halves: n=0 (values 0-127), n=1 (values 128-255)
      var qlOffset = 0
      var qhOffset = 0
      var scOffset = 0
      var yOffset = 0
      
      for n <- 0 until 2 do
        // Process 128 values in this half
        for l <- 0 until 32 do
          val is = l / 16  // Scale index within this 128-value block
          
          // Read ql values
          val ql0 = buf.get(blockStart + qlOffset + l) & 0xFF
          val ql32 = buf.get(blockStart + qlOffset + l + 32) & 0xFF
          
          // Read qh value
          val qhVal = buf.get(blockStart + 128 + qhOffset + l) & 0xFF
          
          // Read scales (int8, so need sign extension)
          val sc0 = buf.get(blockStart + 192 + scOffset + is + 0).toInt
          val sc2 = buf.get(blockStart + 192 + scOffset + is + 2).toInt
          val sc4 = buf.get(blockStart + 192 + scOffset + is + 4).toInt
          val sc6 = buf.get(blockStart + 192 + scOffset + is + 6).toInt
          
          // Compute 4 quantized values
          val q1 = ((ql0 & 0x0F) | (((qhVal >> 0) & 3) << 4)) - 32
          val q2 = ((ql32 & 0x0F) | (((qhVal >> 2) & 3) << 4)) - 32
          val q3 = ((ql0 >> 4) | (((qhVal >> 4) & 3) << 4)) - 32
          val q4 = ((ql32 >> 4) | (((qhVal >> 6) & 3) << 4)) - 32
          
          // Store 4 dequantized values
          result(blockResultStart + yOffset + l + 0) = d * sc0 * q1
          result(blockResultStart + yOffset + l + 32) = d * sc2 * q2
          result(blockResultStart + yOffset + l + 64) = d * sc4 * q3
          result(blockResultStart + yOffset + l + 96) = d * sc6 * q4
        
        // Move to next half
        qlOffset += 64
        qhOffset += 32
        scOffset += 8
        yOffset += 128
    
    result

  /** Dequantize F16 to F32. */
  def dequantizeF16(data: Array[Byte], numElements: Long): Array[Float] =
    val result = new Array[Float](numElements.toInt)
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    
    for i <- 0 until numElements.toInt do
      result(i) = fp16ToFp32(buf.getShort(i * 2))
    
    result
