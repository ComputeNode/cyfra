package io.computenode.cyfra.llama.programs.f32

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct.Empty

/** Q4_K Matrix-Vector Multiplication with inline GPU dequantization.
  *
  * This is the high-performance version that keeps weights quantized on GPU.
  * 8x memory bandwidth reduction vs F32.
  *
  * Q4_K format (144 bytes per 256-element super-block):
  *   - 2 bytes: d (fp16) - main scale
  *   - 2 bytes: dmin (fp16) - min scale
  *   - 12 bytes: scales (6-bit packed, 8 scales + 8 mins)
  *   - 128 bytes: quantized values (4-bit packed, 256 values)
  */
object Q4KMatmulVecProgram:
  val WARP_SIZE = 32
  val QK_K = 256        // Elements per quantization super-block
  val BLOCK_BYTES = 144 // Bytes per Q4_K block
  val UINT32_PER_BLOCK = 36 // uint32 per block (144/4)
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

  case class ProgramLayout(
    weight: GBuffer[UInt32],    // Packed Q4_K: [outFeatures * numQBlocks * 36]
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
    // For denormalized (exp == 0): (mant/1024) * 2^(-14) = mant * 2^(-24)
    // For zero: 0
    val expIsZero = exp === zero
    val mantIsZero = mant === zero
    
    val normMantF = 1.0f + mant.asFloat / 1024.0f
    val normExpF = exp.signed - 15
    val normResult = normMantF * pow(2.0f, normExpF.asFloat)
    val denormResult = mant.asFloat * 5.9604645e-8f  // 2^(-24)
    
    val magnitude = when(expIsZero):
      when(mantIsZero)(0.0f).otherwise(denormResult)
    .otherwise:
      normResult
    
    when(sign === mask1)(-magnitude).otherwise(magnitude)

  def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
    val inFeatures = sizes.inFeatures
    val outFeatures = sizes.outFeatures
    val numQBlocks = sizes.numQBlocks

    GProgram[Sizes, ProgramLayout](
      layout = s => ProgramLayout(
        weight = GBuffer[UInt32](s.outFeatures * s.numQBlocks * UINT32_PER_BLOCK),
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

      // Each warp computes one output
      val outputIdx = workgroupId * NUM_ROWS + warpInWorkgroup
      val batch = outputIdx / outFeaturesVal
      val outIdx = outputIdx.mod(outFeaturesVal)

      // Process all Q4_K blocks for this output row
      // Each lane handles elements laneId, laneId+32, ... within each block
      // Using foldRepeat over Q-blocks (compile-time count)
      val partialSum = GIO.foldRepeat[Float32](numQBlocks, 0.0f): (qBlock, acc) =>
        val blockBase = (outIdx * numQBlocksVal + qBlock) * UINT32_PER_BLOCK
        val colBase = qBlock * QK_K

        // Read d and dmin from first uint32 (two fp16 packed)
        val dminPacked = GIO.read[UInt32](layout.weight, blockBase)
        val mask16: UInt32 = 0xFFFF
        val d = fp16ToFp32(dminPacked & mask16)
        val dmin = fp16ToFp32((dminPacked >> 16.unsigned) & mask16)

        // Each lane processes 8 elements: laneId, laneId+32, ...
        // Use GSeq with unroll hint to generate compact [[unroll]] loop instead of massive inline expansion
        val blockSum = GSeq
          .gen[Int32](0, _ + 1)
          .map { i =>
            val localIdx = laneId + i * 32
            processQ4KElement(layout, blockBase, colBase, batch, inFeaturesVal, d, dmin, localIdx)
          }
          .limit(8)
          .unroll  // Generates [[unroll]] pragma - keeps code compact
          .fold(0.0f, (acc: Float32, x: Float32) => acc + x)
        
        for _ <- GIO.barrier yield
          acc + blockSum

      // IMPORTANT: subgroupAdd must be called by ALL lanes (it's a collective operation)
      // Do NOT wrap subgroupAdd in GIO.when - that breaks the collective reduction!
      partialSum.flatMap: ps =>
        val totalSum: Float32 = GIO.subgroupAdd(ps)
        // All lanes write (safe, coalesced) - like F32 MatmulVecProgram
        // Guard only the write, not the subgroupAdd
        GIO.when(outputIdx < totalOutputsVal):
          GIO.write[Float32](layout.output, outputIdx, totalSum)

  private def processQ4KElement(
    layout: ProgramLayout,
    blockBase: Int32,
    colBase: Int32,
    batch: Int32,
    inFeaturesVal: Int32,
    d: Float32,
    dmin: Float32,
    localIdx: Int32,
  ): Float32 =
    processQ4KElementGeneric(layout.weight, layout.input, blockBase, colBase, batch, inFeaturesVal, d, dmin, localIdx)

  /** Generic Q4K element processor that works with any UInt32 weight buffer.
    * 
    * Matches llama.cpp dequant_q4_k.comp exactly:
    * - 256 elements per block, processed in 8 sub-blocks of 32
    * - subBlock = localIdx / 32 (0-7), is the scale index
    * - Elements in even sub-blocks (0,2,4,6) use LOW nibble
    * - Elements in odd sub-blocks (1,3,5,7) use HIGH nibble
    * - Scale/min extraction uses is < 4 vs is >= 4 logic (NOT even/odd!)
    * 
    * Q4_K layout (144 bytes):
    *   bytes 0-1: d (fp16)
    *   bytes 2-3: dmin (fp16)
    *   bytes 4-15: scales (12 bytes, complex 6-bit packing)
    *   bytes 16-143: qs (128 bytes, 4-bit quantized values)
    * 
    * Scale extraction (from llama.cpp get_scale_min_k4):
    *   For is < 4: sc = scales[is] & 0x3F, m = scales[is+4] & 0x3F
    *   For is >= 4: sc = (scales[is+4] & 0x0F) | ((scales[is-4] >> 6) << 4)
    *                m = ((scales[is+4] >> 4) & 0x0F) | ((scales[is] >> 6) << 4)
    */
  private[f32] def processQ4KElementGeneric(
    weight: GBuffer[UInt32],
    input: GBuffer[Float32],
    blockBase: Int32,
    colBase: Int32,
    batch: Int32,
    inFeaturesVal: Int32,
    d: Float32,
    dmin: Float32,
    localIdx: Int32,
  ): Float32 =
    val valid = localIdx < QK_K
    
    // Map localIdx to sub-block structure
    val subBlock = localIdx / 32      // 0-7 (is)
    val posInSub = localIdx.mod(32)   // 0-31
    
    // Is this a high-nibble sub-block? (odd sub-blocks: 1, 3, 5, 7)
    val isHighNibble = (subBlock.mod(2)) === 1
    
    // qs byte index: (subBlock / 2) * 32 + posInSub
    val j: Int32 = subBlock / 2       // 0-3 (which group of 64)
    val qsByteIdx = j * 32 + posInSub
    
    // Read qs byte from bytes 16-143
    val sixteen: Int32 = 16
    val qsOffset: Int32 = sixteen + qsByteIdx
    val qsUint32Idx = qsOffset / 4
    val qsByteInUint32 = qsOffset.mod(4)
    val qsWord = GIO.read[UInt32](weight, blockBase + qsUint32Idx)
    val byteMask: UInt32 = 0xFF
    val nibbleMask: UInt32 = 0x0F
    val qsByte = (qsWord >> (qsByteInUint32 * 8).unsigned) & byteMask
    
    // Extract 4-bit quantized value
    val q = when(isHighNibble)((qsByte >> 4.unsigned) & nibbleMask).otherwise(qsByte & nibbleMask)
    
    // Scale index = subBlock (is = 0-7)
    val is: Int32 = subBlock
    val isLt4 = is < 4
    
    // Helper to read scale byte (scales at bytes 4-15)
    val four: Int32 = 4
    def readScaleByte(scaleIdx: Int32): UInt32 =
      val byteOffset: Int32 = four + scaleIdx
      val uint32Idx = byteOffset / 4
      val byteInUint32 = byteOffset.mod(4)
      val word = GIO.read[UInt32](weight, blockBase + uint32Idx)
      (word >> (byteInUint32 * 8).unsigned) & byteMask
    
    val mask3F: UInt32 = 0x3F
    val mask0F: UInt32 = 0x0F
    
    // Read scale bytes needed for both branches
    // For is < 4: need scales[is] and scales[is+4]
    // For is >= 4: need scales[is+4], scales[is-4], and scales[is]
    val scalesIs = readScaleByte(is)           // scales[is]
    val scalesIsP4 = readScaleByte(is + 4)     // scales[is+4]
    val scalesIsM4 = readScaleByte(is - 4)     // scales[is-4] (only valid when is >= 4)
    
    // Compute scale (sc) and min (m) based on is < 4 vs is >= 4
    // For is < 4: sc = scales[is] & 0x3F, m = scales[is+4] & 0x3F
    val scLt4 = scalesIs & mask3F
    val mLt4 = scalesIsP4 & mask3F
    
    // For is >= 4: sc = (scales[is+4] & 0x0F) | ((scales[is-4] >> 6) << 4)
    //              m = ((scales[is+4] >> 4) & 0x0F) | ((scales[is] >> 6) << 4)
    val scGe4 = (scalesIsP4 & mask0F) | ((scalesIsM4 >> 6.unsigned) << 4.unsigned)
    val mGe4 = ((scalesIsP4 >> 4.unsigned) & mask0F) | ((scalesIs >> 6.unsigned) << 4.unsigned)
    
    val sc = when(isLt4)(scLt4.asFloat).otherwise(scGe4.asFloat)
    val m = when(isLt4)(mLt4.asFloat).otherwise(mGe4.asFloat)
    
    // Dequantize: result = d * sc * q - dmin * m
    val dequantized = d * sc * q.asFloat - dmin * m

    // Read input
    val col = colBase + localIdx
    val inputVal = GIO.read[Float32](input, batch * inFeaturesVal + col)

    when(valid)(dequantized * inputVal).otherwise(0.0f)
  
  // ============= Layered Version for Pipeline =============
  
  /** Layered Q4K matmul with weight offset for pipeline integration.
    * 
    * This version allows concatenated Q4_K weights across layers.
    */
  object Layered:
    case class Sizes(
      batchSize: Int,
      inFeatures: Int,
      outFeatures: Int,
      weightOffsetUint32: Int,  // Offset in uint32 into the concatenated weight buffer
      totalWeightUint32: Int,   // Total size of concatenated weight buffer
    ):
      require(inFeatures % QK_K == 0, s"inFeatures must be multiple of $QK_K")
      def totalOutputs: Int = batchSize * outFeatures
      def numQBlocks: Int = inFeatures / QK_K
      def numWorkgroups: Int = (totalOutputs + NUM_ROWS - 1) / NUM_ROWS
      def uint32PerRow: Int = numQBlocks * UINT32_PER_BLOCK

    case class ProgramLayout(
      weight: GBuffer[UInt32],
      input: GBuffer[Float32],
      output: GBuffer[Float32],
    ) derives Layout

    def forward(sizes: Sizes): GProgram[Sizes, ProgramLayout] =
      GProgram[Sizes, ProgramLayout](
        layout = s => ProgramLayout(
          weight = GBuffer[UInt32](s.totalWeightUint32),
          input = GBuffer[Float32](s.batchSize * s.inFeatures),
          output = GBuffer[Float32](s.totalOutputs),
        ),
        dispatch = (_, s) => StaticDispatch((s.numWorkgroups, 1, 1)),
        workgroupSize = (BLOCK_SIZE, 1, 1),
      ): layout =>
        forwardBody(sizes, layout)
    
    /** Body of the forward pass - can be called from other programs. */
    def forwardBody(sizes: Sizes, layout: ProgramLayout): GIO[Empty] =
      val inFeatures = sizes.inFeatures
      val outFeatures = sizes.outFeatures
      val numQBlocks = sizes.numQBlocks
      val weightOffset = sizes.weightOffsetUint32
      val uint32PerRow = sizes.uint32PerRow
      
      val tid: Int32 = GIO.localInvocationId.x
      val workgroupId: Int32 = GIO.workgroupId.x
      val laneId = tid.mod(WARP_SIZE)
      val warpInWorkgroup = tid / WARP_SIZE
      val inFeaturesVal: Int32 = inFeatures
      val outFeaturesVal: Int32 = outFeatures
      val numQBlocksVal: Int32 = numQBlocks
      val totalOutputsVal: Int32 = sizes.totalOutputs
      val weightOffsetVal: Int32 = weightOffset
      val uint32PerRowVal: Int32 = uint32PerRow

      val outputIdx = workgroupId * NUM_ROWS + warpInWorkgroup
      val batch = outputIdx / outFeaturesVal
      val outIdx = outputIdx.mod(outFeaturesVal)

      val partialSum = GIO.foldRepeat[Float32](numQBlocks, 0.0f): (qBlock, acc) =>
        // Apply weight offset for layered weights
        val blockBase = weightOffsetVal + outIdx * uint32PerRowVal + qBlock * UINT32_PER_BLOCK
        val colBase = qBlock * QK_K

        val dminPacked = GIO.read[UInt32](layout.weight, blockBase)
        val mask16: UInt32 = 0xFFFF
        val d = fp16ToFp32(dminPacked & mask16)
        val dmin = fp16ToFp32((dminPacked >> 16.unsigned) & mask16)

        // Use GSeq with unroll hint for compact code generation
        val blockSum = GSeq
          .gen[Int32](0, _ + 1)
          .map { i =>
            val localIdx = laneId + i * 32
            processQ4KElementGeneric(layout.weight, layout.input, blockBase, colBase, batch, inFeaturesVal, d, dmin, localIdx)
          }
          .limit(8)
          .unroll
          .fold(0.0f, (acc: Float32, x: Float32) => acc + x)
        
        for _ <- GIO.barrier yield
          acc + blockSum

      // IMPORTANT: subgroupAdd must be called by ALL lanes (it's a collective operation)
      partialSum.flatMap: ps =>
        val totalSum: Float32 = GIO.subgroupAdd(ps)
        GIO.when(outputIdx < totalOutputsVal):
          GIO.write[Float32](layout.output, outputIdx, totalSum)
