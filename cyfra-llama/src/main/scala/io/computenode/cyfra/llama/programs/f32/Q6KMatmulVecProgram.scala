package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct.Empty

/** Q6_K Matrix-Vector Multiplication with inline GPU dequantization.
  *
  * Q6_K format (210 bytes per 256-element super-block):
  *   - 128 bytes: ql - low 4 bits (packed 2 per byte)
  *   - 64 bytes: qh - high 2 bits (packed 4 per byte)
  *   - 16 bytes: scales (int8, one per 16 elements)
  *   - 2 bytes: d (fp16) - main scale
  */
object Q6KMatmulVecProgram:
  val WARP_SIZE = 32
  val QK_K = 256        // Elements per quantization super-block
  val BLOCK_BYTES = 210 // Bytes per Q6_K block
  val UINT32_PER_BLOCK = 53 // ceil(210/4) - actually 52.5, so we use 53 uint32 with padding
  val BLOCK_SIZE = 256  // Workgroup size
  val NUM_ROWS = 8      // Outputs per workgroup

  case class Sizes(
    batchSize: Int,      // B * T
    inFeatures: Int,     // K - must be multiple of QK_K (256)
    outFeatures: Int,    // N
  ):
    require(inFeatures % QK_K == 0, s"inFeatures must be multiple of $QK_K")
    def totalOutputs: Int = batchSize * outFeatures
    def numQBlocks: Int = inFeatures / QK_K
    def numWorkgroups: Int = (totalOutputs + NUM_ROWS - 1) / NUM_ROWS
    // Q6_K uses 210 bytes per block = 52.5 uint32, so we pack at byte level
    def bytesPerRow: Int = numQBlocks * BLOCK_BYTES

  case class ProgramLayout(
    weight: GBuffer[UInt32],    // Packed Q6_K: stored as bytes packed in uint32
    input: GBuffer[Float32],   // [batchSize, inFeatures]
    output: GBuffer[Float32],  // [batchSize, outFeatures]
  ) derives Layout

  /** Convert fp16 (half precision) to fp32.
    * Handles normalized, denormalized, and zero values.
    */
  private def fp16ToFp32(h: UInt32): Float32 =
    val mask1: UInt32 = 1
    val zero: UInt32 = 0
    val expMask: UInt32 = 0x1F
    val mantMask: UInt32 = 0x3FF
    val sign = (h >> 15.unsigned) & mask1
    val exp = (h >> 10.unsigned) & expMask
    val mant = h & mantMask

    // For normalized numbers (exp > 0): (1 + mant/1024) * 2^(exp-15)
    // For denormalized (exp == 0): (mant/1024) * 2^(-14)
    // For zero: 0
    val expIsZero = exp === zero
    val mantIsZero = mant === zero
    
    // Normalized case
    val normMantF = 1.0f + mant.asFloat / 1024.0f
    val normExpF = exp.signed - 15
    val normResult = normMantF * pow(2.0f, normExpF.asFloat)
    
    // Denormalized case: mant/1024 * 2^(-14) = mant * 2^(-24)
    val denormResult = mant.asFloat * 5.9604645e-8f  // 2^(-24)
    
    // Select result based on exp and mant
    val magnitude = when(expIsZero):
      when(mantIsZero)(0.0f).otherwise(denormResult)
    .otherwise:
      normResult
    
    when(sign === mask1)(-magnitude).otherwise(magnitude)
  
  /** Read a byte from the weight buffer at a given byte offset. */
  private def readByte(weight: GBuffer[UInt32], byteOffset: Int32): UInt32 =
    val uint32Idx = byteOffset / 4
    val byteInUint32 = byteOffset.mod(4)
    val word = GIO.read[UInt32](weight, uint32Idx)
    val byteMask: UInt32 = 0xFF
    (word >> (byteInUint32 * 8).unsigned) & byteMask
  
  /** Read a signed byte from the weight buffer. */
  private def readSignedByte(weight: GBuffer[UInt32], byteOffset: Int32): Int32 =
    val b = readByte(weight, byteOffset)
    // Convert unsigned byte to signed: if > 127, subtract 256
    val mask127: UInt32 = 127
    when(b > mask127)(b.signed - 256).otherwise(b.signed)

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val inFeatures = sizes.inFeatures
    val outFeatures = sizes.outFeatures
    val numQBlocks = sizes.numQBlocks
    val bytesPerRow = sizes.bytesPerRow

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        weight = GBuffer[UInt32]((s.outFeatures * s.bytesPerRow + 3) / 4),
        input = GBuffer[Float32](s.batchSize * s.inFeatures),
        output = GBuffer[Float32](s.totalOutputs),
      ),
      dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
      workgroupSize = (BLOCK_SIZE, 1, 1),
    ): layout =>
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val laneId = tid.mod(WARP_SIZE)
      val warpInWorkgroup = tid / WARP_SIZE
      val inFeaturesVal: Int32 = inFeatures
      val outFeaturesVal: Int32 = outFeatures
      val numQBlocksVal: Int32 = numQBlocks
      val totalOutputsVal: Int32 = sizes.totalOutputs
      val bytesPerRowVal: Int32 = bytesPerRow

      // Each warp computes one output
      val outputIdx = workgroupId * NUM_ROWS + warpInWorkgroup
      val batch = outputIdx / outFeaturesVal
      val outIdx = outputIdx.mod(outFeaturesVal)

      // Process all Q6_K blocks for this output row
      val partialSum = GIO.foldRepeat[Float32](numQBlocks, 0.0f): (qBlock, acc) =>
        val blockByteBase = outIdx * bytesPerRowVal + qBlock * BLOCK_BYTES
        val colBase = qBlock * QK_K

        // Q6_K block layout (210 bytes):
        // - ql: bytes 0-127 (low 4 bits)
        // - qh: bytes 128-191 (high 2 bits)
        // - scales: bytes 192-207 (int8)
        // - d: bytes 208-209 (fp16)

        // Read d (fp16 at byte 208)
        val dWord = GIO.read[UInt32](layout.weight, (blockByteBase + 208) / 4)
        val dBytePos = (blockByteBase + 208).mod(4)
        val mask16: UInt32 = 0xFFFF
        val dHalf = (dWord >> (dBytePos * 8).unsigned) & mask16
        val d = fp16ToFp32(dHalf)

        // Each lane processes 8 elements: laneId, laneId+32, ..., laneId+224
        val iter0 = processQ6KElement(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId)
        val iter1 = processQ6KElement(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 32)
        val iter2 = processQ6KElement(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 64)
        val iter3 = processQ6KElement(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 96)
        val iter4 = processQ6KElement(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 128)
        val iter5 = processQ6KElement(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 160)
        val iter6 = processQ6KElement(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 192)
        val iter7 = processQ6KElement(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 224)
        
        for _ <- GIO.barrier yield
          acc + iter0 + iter1 + iter2 + iter3 + iter4 + iter5 + iter6 + iter7

      // IMPORTANT: subgroupAdd must be called by ALL lanes (it's a collective operation)
      partialSum.flatMap: ps =>
        val totalSum: Float32 = GIO.subgroupAdd(ps)
        GIO.when(outputIdx < totalOutputsVal):
          GIO.write[Float32](layout.output, outputIdx, totalSum)

  /** Process a single Q6_K element.
    * 
    * Q6_K dequantization (matches llama.cpp dequant_q6_k.comp exactly):
    * - Block has 256 elements split into 2 halves of 128
    * - For element localIdx in [0, 255]:
    *   - ip = localIdx / 128 (which half: 0 or 1)
    *   - il = localIdx % 128 (position within half: 0-127)
    *   - is = ip * 8 + il / 16 (base scale index)
    *   - y_idx = 128 * ip + il (output position)
    *   - ql_idx = 64 * ip + il (ql byte index for low nibble)
    *   - qh is at: 32 * ip + il (one byte per pair of ql elements)
    * 
    * The encoding:
    *   - Output[y_idx + 0]: ql[ql_idx] low nibble + qh bits 0-1
    *   - Output[y_idx + 32]: ql[ql_idx + 32] low nibble + qh bits 2-3
    *   - Output[y_idx + 64]: ql[ql_idx] high nibble + qh bits 4-5
    *   - Output[y_idx + 96]: ql[ql_idx + 32] high nibble + qh bits 6-7
    * 
    * Since we process elements sequentially (localIdx = 0..255), we need
    * to reverse-map from localIdx to determine which case we're in.
    */
  private def processQ6KElement(
    weight: GBuffer[UInt32],
    input: GBuffer[Float32],
    blockByteBase: Int32,
    colBase: Int32,
    batch: Int32,
    inFeaturesVal: Int32,
    d: Float32,
    localIdx: Int32,
  ): Float32 =
    val valid = localIdx < QK_K
    
    // Determine which half (ip) and position within the 128-value range
    val ip = localIdx / 128       // 0 or 1
    val posIn128 = localIdx.mod(128)  // 0-127
    
    // Each 128-value half is organized as 4 groups of 32:
    // Group 0: positions 0-31 (y_idx + 0)
    // Group 1: positions 32-63 (y_idx + 32)
    // Group 2: positions 64-95 (y_idx + 64)
    // Group 3: positions 96-127 (y_idx + 96)
    val group = posIn128 / 32     // 0, 1, 2, or 3
    val il = posIn128.mod(32)     // 0-31 (position within group)
    
    // Compute ql_idx and qh_idx (the "source" indices in llama.cpp)
    val ql_idx_base = ip * 64 + il
    val qh_idx = ip * 32 + il
    
    // Read the qh byte
    val qhByteOffset = blockByteBase + 128 + qh_idx
    val qhByte = readByte(weight, qhByteOffset)
    
    // Determine which ql byte and nibble based on group
    // Group 0: ql[ql_idx + 0], low nibble, qh bits 0-1
    // Group 1: ql[ql_idx + 32], low nibble, qh bits 2-3
    // Group 2: ql[ql_idx + 0], high nibble, qh bits 4-5
    // Group 3: ql[ql_idx + 32], high nibble, qh bits 6-7
    val ql_idx = when(group === 0 || group === 2)(ql_idx_base).otherwise(ql_idx_base + 32)
    val useHighNibble = group >= 2
    val qhShift = group * 2
    
    val qlByteOffset = blockByteBase + ql_idx
    val qlByte = readByte(weight, qlByteOffset)
    val nibbleMask: UInt32 = 0x0F
    val ql = when(useHighNibble)((qlByte >> 4.unsigned) & nibbleMask).otherwise(qlByte & nibbleMask)
    
    val qhMask: UInt32 = 0x03
    val qh = (qhByte >> qhShift.unsigned) & qhMask
    
    // Combine to get 6-bit value and subtract 32 for signed
    val q6 = ql | (qh << 4.unsigned)
    val qSigned = q6.signed - 32
    
    // Compute scale index: is = 8 * ip + il / 16
    // For each group, we use a different scale offset:
    // Group 0 uses is + 0, Group 1 uses is + 2, Group 2 uses is + 4, Group 3 uses is + 6
    val is_base = ip * 8 + il / 16
    val scaleOffset = group * 2
    val scaleIdx = is_base + scaleOffset
    val scaleByteOffset = blockByteBase + 192 + scaleIdx
    val scale = readSignedByte(weight, scaleByteOffset)
    
    // Dequantize
    val dequantized = d * scale.asFloat * qSigned.asFloat

    // Read input
    val col = colBase + localIdx
    val inputVal = GIO.read[Float32](input, batch * inFeaturesVal + col)

    when(valid)(dequantized * inputVal).otherwise(0.0f)
  
  // ============= Layered Version for Pipeline =============
  
  object Layered:
    case class Sizes(
      batchSize: Int,
      inFeatures: Int,
      outFeatures: Int,
      weightOffsetBytes: Int,  // Offset in BYTES into the concatenated weight buffer
      totalWeightBytes: Int,   // Total size of concatenated weight buffer in bytes
    ):
      require(inFeatures % QK_K == 0, s"inFeatures must be multiple of $QK_K")
      def totalOutputs: Int = batchSize * outFeatures
      def numQBlocks: Int = inFeatures / QK_K
      def numWorkgroups: Int = (totalOutputs + NUM_ROWS - 1) / NUM_ROWS
      def bytesPerRow: Int = numQBlocks * BLOCK_BYTES

    case class ProgramLayout(
      weight: GBuffer[UInt32],
      input: GBuffer[Float32],
      output: GBuffer[Float32],
    ) derives Layout

    def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
      GProgram[Sizes, ProgramLayout](
        layout = s => ProgramLayout(
          weight = GBuffer[UInt32]((s.totalWeightBytes + 3) / 4),
          input = GBuffer[Float32](s.batchSize * s.inFeatures),
          output = GBuffer[Float32](s.totalOutputs),
        ),
        dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
        workgroupSize = (BLOCK_SIZE, 1, 1),
      ): layout =>
        forwardBody(sizes, layout)
    
    def forwardBody(sizes: Sizes, layout: ProgramLayout): GIO[Empty] =
      val inFeatures = sizes.inFeatures
      val outFeatures = sizes.outFeatures
      val numQBlocks = sizes.numQBlocks
      val weightOffsetBytes = sizes.weightOffsetBytes
      val bytesPerRow = sizes.bytesPerRow
      
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val laneId = tid.mod(WARP_SIZE)
      val warpInWorkgroup = tid / WARP_SIZE
      val inFeaturesVal: Int32 = inFeatures
      val outFeaturesVal: Int32 = outFeatures
      val numQBlocksVal: Int32 = numQBlocks
      val totalOutputsVal: Int32 = sizes.totalOutputs
      val weightOffsetBytesVal: Int32 = weightOffsetBytes
      val bytesPerRowVal: Int32 = bytesPerRow

      val outputIdx = workgroupId * NUM_ROWS + warpInWorkgroup
      val batch = outputIdx / outFeaturesVal
      val outIdx = outputIdx.mod(outFeaturesVal)

      val partialSum = GIO.foldRepeat[Float32](numQBlocks, 0.0f): (qBlock, acc) =>
        val blockByteBase = weightOffsetBytesVal + outIdx * bytesPerRowVal + qBlock * BLOCK_BYTES
        val colBase = qBlock * QK_K

        val dWord = GIO.read[UInt32](layout.weight, (blockByteBase + 208) / 4)
        val dBytePos = (blockByteBase + 208).mod(4)
        val mask16: UInt32 = 0xFFFF
        val dHalf = (dWord >> (dBytePos * 8).unsigned) & mask16
        val d = fp16ToFp32(dHalf)

        val iter0 = processQ6KElementGeneric(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId)
        val iter1 = processQ6KElementGeneric(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 32)
        val iter2 = processQ6KElementGeneric(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 64)
        val iter3 = processQ6KElementGeneric(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 96)
        val iter4 = processQ6KElementGeneric(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 128)
        val iter5 = processQ6KElementGeneric(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 160)
        val iter6 = processQ6KElementGeneric(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 192)
        val iter7 = processQ6KElementGeneric(layout.weight, layout.input, blockByteBase, colBase, batch, inFeaturesVal, d, laneId + 224)
        
        for _ <- GIO.barrier yield
          acc + iter0 + iter1 + iter2 + iter3 + iter4 + iter5 + iter6 + iter7

      // IMPORTANT: subgroupAdd must be called by ALL lanes (it's a collective operation)
      partialSum.flatMap: ps =>
        val totalSum: Float32 = GIO.subgroupAdd(ps)
        GIO.when(outputIdx < totalOutputsVal):
          GIO.write[Float32](layout.output, outputIdx, totalSum)

  private def readByteGeneric(weight: GBuffer[UInt32], byteOffset: Int32): UInt32 =
    val uint32Idx = byteOffset / 4
    val byteInUint32 = byteOffset.mod(4)
    val word = GIO.read[UInt32](weight, uint32Idx)
    val byteMask: UInt32 = 0xFF
    (word >> (byteInUint32 * 8).unsigned) & byteMask
  
  private def readSignedByteGeneric(weight: GBuffer[UInt32], byteOffset: Int32): Int32 =
    val b = readByteGeneric(weight, byteOffset)
    val mask127: UInt32 = 127
    when(b > mask127)(b.signed - 256).otherwise(b.signed)

  /** Process a single Q6_K element (matches llama.cpp dequant_q6_k.comp). */
  private def processQ6KElementGeneric(
    weight: GBuffer[UInt32],
    input: GBuffer[Float32],
    blockByteBase: Int32,
    colBase: Int32,
    batch: Int32,
    inFeaturesVal: Int32,
    d: Float32,
    localIdx: Int32,
  ): Float32 =
    val valid = localIdx < QK_K
    
    // Determine which half (ip) and position within the 128-value range
    val ip = localIdx / 128       // 0 or 1
    val posIn128 = localIdx.mod(128)  // 0-127
    
    // Each 128-value half is organized as 4 groups of 32
    val group = posIn128 / 32     // 0, 1, 2, or 3
    val il = posIn128.mod(32)     // 0-31 (position within group)
    
    // Compute ql_idx and qh_idx
    val ql_idx_base = ip * 64 + il
    val qh_idx = ip * 32 + il
    
    // Read the qh byte
    val qhByteOffset = blockByteBase + 128 + qh_idx
    val qhByte = readByteGeneric(weight, qhByteOffset)
    
    // Determine which ql byte and nibble based on group
    val ql_idx = when(group === 0 || group === 2)(ql_idx_base).otherwise(ql_idx_base + 32)
    val useHighNibble = group >= 2
    val qhShift = group * 2
    
    val qlByteOffset = blockByteBase + ql_idx
    val qlByte = readByteGeneric(weight, qlByteOffset)
    val nibbleMask: UInt32 = 0x0F
    val ql = when(useHighNibble)((qlByte >> 4.unsigned) & nibbleMask).otherwise(qlByte & nibbleMask)
    
    val qhMask: UInt32 = 0x03
    val qh = (qhByte >> qhShift.unsigned) & qhMask
    
    // Combine to get 6-bit value and subtract 32 for signed
    val q6 = ql | (qh << 4.unsigned)
    val qSigned = q6.signed - 32
    
    // Compute scale index
    val is_base = ip * 8 + il / 16
    val scaleOffset = group * 2
    val scaleIdx = is_base + scaleOffset
    val scaleByteOffset = blockByteBase + 192 + scaleIdx
    val scale = readSignedByteGeneric(weight, scaleByteOffset)
    
    val dequantized = d * scale.asFloat * qSigned.asFloat

    val col = colBase + localIdx
    val inputVal = GIO.read[Float32](input, batch * inFeaturesVal + col)

    when(valid)(dequantized * inputVal).otherwise(0.0f)
