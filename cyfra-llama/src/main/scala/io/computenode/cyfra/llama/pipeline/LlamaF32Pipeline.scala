package io.computenode.cyfra.llama.pipeline

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GCodec, GExecution, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.GShared
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.dsl.given_GStructConstructor_T
import io.computenode.cyfra.llama.model.LlamaConfig
import io.computenode.cyfra.llama.pipeline.PipelineUtils.*
import io.computenode.cyfra.llama.programs.AttentionParams
import io.computenode.cyfra.llama.programs.f32.*
import io.computenode.cyfra.llama.util.Logger

import java.nio.{ByteBuffer, ByteOrder}

/** F32/Quantized Llama GPU Pipeline.
  *
  * Supports quantized weights (Q4_K, Q6_K) with on-GPU dequantization.
  * Uses F32 activations for compute precision.
  *
  * Pattern:
  *   - Single GExecution covering ALL operations
  *   - All layer parameters concatenated in single buffers
  *   - Layer offsets computed at compile time per-program
  *   - ByteBuffer-based I/O for efficiency
  */
object LlamaF32Pipeline:

  // ============= Params =============

  /** Unified params struct for Llama programs. */
  case class LlamaParams(
    B: Int32,           // batch size
    T: Int32,           // sequence length
    C: Int32,           // hidden size
    NH: Int32,          // number of query heads
    NKV: Int32,         // number of key-value heads
    headSize: Int32,    // head size
    FFN: Int32,         // intermediate (FFN) size
    V: Int32,           // vocab size
    L: Int32,           // total layers
    eps: Float32,       // RMSNorm epsilon
    ropeTheta: Float32, // RoPE theta
    startPos: Int32,    // RoPE starting position
  ) extends GStruct[LlamaParams]

  object LlamaParams:
    def create(config: LlamaConfig, T: Int, B: Int = 1, startPos: Int = 0): LlamaParams =
      LlamaParams(
        B = B,
        T = T,
        C = config.hiddenSize,
        NH = config.numAttentionHeads,
        NKV = config.numKeyValueHeads,
        headSize = config.headSize,
        FFN = config.intermediateSize,
        V = config.vocabSize,
        L = config.numHiddenLayers,
        eps = config.rmsNormEps.toFloat,
        ropeTheta = config.ropeTheta.toFloat,
        startPos = startPos,
      )

  case class PipelineParams(
    config: LlamaConfig,
    B: Int,
    T: Int,
    startPos: Int = 0,
  )

  // ============= Weight Types =============
  
  /** Quantization type indicator for weights. */
  sealed trait QuantWeightType
  case object Q4K extends QuantWeightType
  case object Q6K extends QuantWeightType
  
  /** Mixed quantized layer weights - supports both Q4_K and Q6_K.
    * TinyLlama uses Q4_K for most weights but Q6_K for V tensors.
    */
  case class MixedQuantLayerWeights(
    attnNorm: Array[Float],    // F32 - norms stay F32
    wq: Array[Int],            // Quantized (Q4_K or Q6_K)
    wqType: QuantWeightType,
    wk: Array[Int],            // Quantized
    wkType: QuantWeightType,
    wv: Array[Int],            // Quantized (often Q6_K in TinyLlama)
    wvType: QuantWeightType,
    wo: Array[Int],            // Quantized
    woType: QuantWeightType,
    ffnNorm: Array[Float],     // F32 - norms stay F32
    ffnGate: Array[Int],       // Quantized
    ffnGateType: QuantWeightType,
    ffnUp: Array[Int],         // Quantized
    ffnUpType: QuantWeightType,
    ffnDown: Array[Int],       // Quantized
    ffnDownType: QuantWeightType,
  )

  case class MixedQuantModelWeights(
    tokenEmbed: Array[Float],  // F32 - embeddings stay F32
    layers: Seq[MixedQuantLayerWeights],
    outputNorm: Array[Float],  // F32
    output: Array[Float],      // F32 - output head stays F32
  )
  
  
  /** Calculate Q4_K size in uint32 for a weight matrix. */
  def q4kSizeUint32(outFeatures: Int, inFeatures: Int): Int =
    val numQBlocks = inFeatures / Q4KMatmulVecProgram.QK_K
    outFeatures * numQBlocks * Q4KMatmulVecProgram.UINT32_PER_BLOCK
  
  /** Calculate Q6_K size in bytes for a weight matrix. */
  def q6kSizeBytes(outFeatures: Int, inFeatures: Int): Int =
    val numQBlocks = inFeatures / Q6KMatmulVecProgram.QK_K
    outFeatures * numQBlocks * Q6KMatmulVecProgram.BLOCK_BYTES

  // ============= KV-Cached Pipeline Layout =============
  
  /** Pipeline layout with KV cache for incremental generation.
    * 
    * The KV cache stores K,V vectors for all previous positions, enabling O(1) decode.
    * Layout structure follows llama.cpp:
    *   - kCache/vCache: (L, maxSeqLen, NKV, headSize) - persistent across forward calls
    *   - Other buffers: sized for current T (can be 1 for decode)
    */
  case class KVCachePipelineLayout(
    // Input
    tokens: GBuffer[Int32],
    
    // Parameters - F32 (embeddings, norms)
    tokenEmbed: GBuffer[Float32],
    attnNorm: GBuffer[Float32],
    ffnNorm: GBuffer[Float32],
    outputNorm: GBuffer[Float32],
    outputWeight: GBuffer[Float32],
    
    // Parameters - Quantized (Q4_K/Q6_K)
    wq: GBuffer[UInt32],
    wk: GBuffer[UInt32],
    wo: GBuffer[UInt32],
    ffnGate: GBuffer[UInt32],
    ffnUp: GBuffer[UInt32],
    ffnDown: GBuffer[UInt32],
    wv: GBuffer[UInt32],  // May be Q6_K
    
    // KV Cache - persistent across calls (L * maxSeqLen * kvSize)
    kCache: GBuffer[Float32],  // (L, maxSeqLen, NKV, headSize)
    vCache: GBuffer[Float32],  // (L, maxSeqLen, NKV, headSize)
    
    // Activations - sized for current T
    hidden: GBuffer[Float32],
    residual: GBuffer[Float32],
    attnNormOut: GBuffer[Float32],
    q: GBuffer[Float32],
    k: GBuffer[Float32],
    v: GBuffer[Float32],
    qRoped: GBuffer[Float32],
    kRoped: GBuffer[Float32],
    attnOut: GBuffer[Float32],
    ffnNormOut: GBuffer[Float32],
    gate: GBuffer[Float32],
    up: GBuffer[Float32],
    ffnHidden: GBuffer[Float32],
    ffnOut: GBuffer[Float32],
    logits: GBuffer[Float32],
    
    // Params uniforms
    params: GUniform[LlamaParams],
    attnParams: GUniform[AttentionParams],  // Runtime seqLen for attention
  ) derives Layout
  
  /** Combined layout for efficient generation - contains BOTH prefill and decode buffers.
    * 
    * This allows a single runUnsafe where:
    * - Weights are uploaded ONCE
    * - KV cache stays on GPU
    * - Prefill uses prefill-sized activations
    * - Decode uses decode-sized activations
    */
  case class GenerationLayout(
    // === Shared: Weights (uploaded once, read-only) ===
    tokenEmbed: GBuffer[Float32],
    attnNorm: GBuffer[Float32],
    ffnNorm: GBuffer[Float32],
    outputNorm: GBuffer[Float32],
    outputWeight: GBuffer[Float32],
    wq: GBuffer[UInt32],
    wk: GBuffer[UInt32],
    wo: GBuffer[UInt32],
    ffnGate: GBuffer[UInt32],
    ffnUp: GBuffer[UInt32],
    ffnDown: GBuffer[UInt32],
    wv: GBuffer[UInt32],
    
    // === Shared: KV Cache (persists across prefill and decode) ===
    kCache: GBuffer[Float32],
    vCache: GBuffer[Float32],
    
    // === Prefill I/O and activations (T = promptLen) ===
    prefillTokens: GBuffer[Int32],
    prefillHidden: GBuffer[Float32],
    prefillResidual: GBuffer[Float32],
    prefillAttnNormOut: GBuffer[Float32],
    prefillQ: GBuffer[Float32],
    prefillK: GBuffer[Float32],
    prefillV: GBuffer[Float32],
    prefillQRoped: GBuffer[Float32],
    prefillKRoped: GBuffer[Float32],
    prefillAttnOut: GBuffer[Float32],
    prefillFfnNormOut: GBuffer[Float32],
    prefillGate: GBuffer[Float32],
    prefillUp: GBuffer[Float32],
    prefillFfnHidden: GBuffer[Float32],
    prefillFfnOut: GBuffer[Float32],
    prefillLogits: GBuffer[Float32],
    prefillParams: GUniform[LlamaParams],
    prefillAttnParams: GUniform[AttentionParams],
    
    // === Decode I/O and activations (T = 1) ===
    decodeToken: GBuffer[Int32],
    decodeHidden: GBuffer[Float32],
    decodeResidual: GBuffer[Float32],
    decodeAttnNormOut: GBuffer[Float32],
    decodeQ: GBuffer[Float32],
    decodeK: GBuffer[Float32],
    decodeV: GBuffer[Float32],
    decodeQRoped: GBuffer[Float32],
    decodeKRoped: GBuffer[Float32],
    decodeAttnOut: GBuffer[Float32],
    decodeFfnNormOut: GBuffer[Float32],
    decodeGate: GBuffer[Float32],
    decodeUp: GBuffer[Float32],
    decodeFfnHidden: GBuffer[Float32],
    decodeFfnOut: GBuffer[Float32],
    decodeLogits: GBuffer[Float32],
    decodeParams: GUniform[LlamaParams],
    decodeAttnParams: GUniform[AttentionParams],
  ) derives Layout:
    /** Map to KVCachePipelineLayout for prefill */
    def toPrefillLayout: KVCachePipelineLayout = KVCachePipelineLayout(
      tokens = prefillTokens,
      tokenEmbed = tokenEmbed, attnNorm = attnNorm, ffnNorm = ffnNorm,
      outputNorm = outputNorm, outputWeight = outputWeight,
      wq = wq, wk = wk, wo = wo, ffnGate = ffnGate, ffnUp = ffnUp, ffnDown = ffnDown, wv = wv,
      kCache = kCache, vCache = vCache,
      hidden = prefillHidden, residual = prefillResidual, attnNormOut = prefillAttnNormOut,
      q = prefillQ, k = prefillK, v = prefillV, qRoped = prefillQRoped, kRoped = prefillKRoped,
      attnOut = prefillAttnOut, ffnNormOut = prefillFfnNormOut,
      gate = prefillGate, up = prefillUp, ffnHidden = prefillFfnHidden, ffnOut = prefillFfnOut,
      logits = prefillLogits, params = prefillParams, attnParams = prefillAttnParams,
    )
    
    /** Map to KVCachePipelineLayout for decode */
    def toDecodeLayout: KVCachePipelineLayout = KVCachePipelineLayout(
      tokens = decodeToken,
      tokenEmbed = tokenEmbed, attnNorm = attnNorm, ffnNorm = ffnNorm,
      outputNorm = outputNorm, outputWeight = outputWeight,
      wq = wq, wk = wk, wo = wo, ffnGate = ffnGate, ffnUp = ffnUp, ffnDown = ffnDown, wv = wv,
      kCache = kCache, vCache = vCache,
      hidden = decodeHidden, residual = decodeResidual, attnNormOut = decodeAttnNormOut,
      q = decodeQ, k = decodeK, v = decodeV, qRoped = decodeQRoped, kRoped = decodeKRoped,
      attnOut = decodeAttnOut, ffnNormOut = decodeFfnNormOut,
      gate = decodeGate, up = decodeUp, ffnHidden = decodeFfnHidden, ffnOut = decodeFfnOut,
      logits = decodeLogits, params = decodeParams, attnParams = decodeAttnParams,
    )

  // ============= KV-Cached Pipeline Class =============

  /** KV-Cached Pipeline for fast incremental inference.
    * 
    * Like llama.cpp, maintains persistent KV cache buffers:
    *   - Prefill: Process all prompt tokens at once, fill KV cache from 0 to T-1
    *   - Decode: Process 1 token at a time, append to KV cache at seqLen, attend to full cache
    * 
    * This achieves O(1) complexity per generated token (vs O(T) without cache).
    */
  class F32KVCachedPipeline(
    weights: MixedQuantModelWeights,
    val config: LlamaConfig,
    maxSeqLen: Int = KVCachedAttention.MAX_SEQ_LEN,
    B: Int = 1,
  )(using runtime: CyfraRuntime) extends LlamaPipeline:
    require(maxSeqLen <= KVCachedAttention.MAX_SEQ_LEN, 
      s"maxSeqLen=$maxSeqLen exceeds KVCachedAttention.MAX_SEQ_LEN=${KVCachedAttention.MAX_SEQ_LEN}")
    
    private val C = config.hiddenSize
    private val NH = config.numAttentionHeads
    private val NKV = config.numKeyValueHeads
    private val headSize = config.headSize
    private val FFN = config.intermediateSize
    private val V = config.vocabSize
    private val L = config.numHiddenLayers
    private val kvSize = NKV * headSize
    
    // Q4_K sizes per tensor (in uint32)
    private val wqUint32PerLayer = q4kSizeUint32(C, C)
    private val wkUint32PerLayer = q4kSizeUint32(kvSize, C)
    private val woUint32PerLayer = q4kSizeUint32(C, C)
    private val ffnGateUint32PerLayer = q4kSizeUint32(FFN, C)
    private val ffnUpUint32PerLayer = q4kSizeUint32(FFN, C)
    
    // Per-layer weight type info from loaded weights
    private val vTypes: Seq[QuantWeightType] = weights.layers.map(_.wvType)
    private val downTypes: Seq[QuantWeightType] = weights.layers.map(_.ffnDownType)
    
    // Per-layer sizes for V and down (varies by quant type)
    private val wvQ4kUint32 = q4kSizeUint32(kvSize, C)
    private val wvQ6kBytes = q6kSizeBytes(kvSize, C)
    private val wvQ6kUint32 = (wvQ6kBytes + 3) / 4
    
    private val downQ4kUint32 = q4kSizeUint32(C, FFN)
    private val downQ6kBytes = q6kSizeBytes(C, FFN)
    private val downQ6kUint32 = (downQ6kBytes + 3) / 4
    
    // Calculate cumulative offsets for mixed-type buffers
    private val vOffsets: Seq[Int] = weights.layers.scanLeft(0) { (offset, layer) =>
      offset + (if layer.wvType == Q6K then wvQ6kUint32 else wvQ4kUint32)
    }.dropRight(1)
    private val totalVUint32 = vOffsets.lastOption.getOrElse(0) + 
      (if vTypes.lastOption.contains(Q6K) then wvQ6kUint32 else wvQ4kUint32)
    
    private val downOffsets: Seq[Int] = weights.layers.scanLeft(0) { (offset, layer) =>
      offset + (if layer.ffnDownType == Q6K then downQ6kUint32 else downQ4kUint32)
    }.dropRight(1)
    private val totalDownUint32 = downOffsets.lastOption.getOrElse(0) +
      (if downTypes.lastOption.contains(Q6K) then downQ6kUint32 else downQ4kUint32)
    
    Logger.info(s"Uploading F32/quantized weights: ${L} layers, ${V}×${C} vocab, maxSeqLen=$maxSeqLen")
    
    private val tokenEmbedBuf = allocateF32Buffer(V * C); copyToF32Buffer(weights.tokenEmbed, tokenEmbedBuf)
    private val attnNormBuf = allocateF32Buffer(L * C)
    private val ffnNormBuf = allocateF32Buffer(L * C)
    private val outputNormBuf = allocateF32Buffer(C); copyToF32Buffer(weights.outputNorm, outputNormBuf)
    private val outputWeightBuf = allocateF32Buffer(V * C); copyToF32Buffer(weights.output, outputWeightBuf)
    
    // Fill norm buffers
    for (layer, layerIdx) <- weights.layers.zipWithIndex do
      attnNormBuf.position(layerIdx * C * 4).asFloatBuffer().put(layer.attnNorm)
      ffnNormBuf.position(layerIdx * C * 4).asFloatBuffer().put(layer.ffnNorm)
    attnNormBuf.rewind(); ffnNormBuf.rewind()
    
    // Pre-allocate quantized weight buffers
    private val wqBuf = allocateIntBuffer(L * wqUint32PerLayer)
    private val wkBuf = allocateIntBuffer(L * wkUint32PerLayer)
    private val woBuf = allocateIntBuffer(L * woUint32PerLayer)
    private val ffnGateBuf = allocateIntBuffer(L * ffnGateUint32PerLayer)
    private val ffnUpBuf = allocateIntBuffer(L * ffnUpUint32PerLayer)
    private val wvBuf = allocateIntBuffer(totalVUint32)
    private val ffnDownBuf = allocateIntBuffer(totalDownUint32)
    
    // Fill weight buffers
    for (layer, layerIdx) <- weights.layers.zipWithIndex do
      wqBuf.position(layerIdx * wqUint32PerLayer * 4).asIntBuffer().put(layer.wq)
      wkBuf.position(layerIdx * wkUint32PerLayer * 4).asIntBuffer().put(layer.wk)
      woBuf.position(layerIdx * woUint32PerLayer * 4).asIntBuffer().put(layer.wo)
      ffnGateBuf.position(layerIdx * ffnGateUint32PerLayer * 4).asIntBuffer().put(layer.ffnGate)
      ffnUpBuf.position(layerIdx * ffnUpUint32PerLayer * 4).asIntBuffer().put(layer.ffnUp)
      wvBuf.position(vOffsets(layerIdx) * 4).asIntBuffer().put(layer.wv)
      ffnDownBuf.position(downOffsets(layerIdx) * 4).asIntBuffer().put(layer.ffnDown)
    
    wqBuf.rewind(); wkBuf.rewind(); woBuf.rewind()
    ffnGateBuf.rewind(); ffnUpBuf.rewind()
    wvBuf.rewind(); ffnDownBuf.rewind()
    
    // Pipeline cache to avoid recompilation
    private val pipelineCache = scala.collection.mutable.Map[(Int, Int), GExecution[PipelineParams, KVCachePipelineLayout, KVCachePipelineLayout]]()
    
    private def getOrBuildPipeline(T: Int, seqLen: Int): GExecution[PipelineParams, KVCachePipelineLayout, KVCachePipelineLayout] =
      pipelineCache.getOrElseUpdate((T, seqLen), buildKVCachedPipeline(T, seqLen))
    
    // Current sequence length (updated after each forward)
    private var currentSeqLen: Int = 0
    
    /** Current position in sequence (for RoPE and masking). */
    def seqLen: Int = currentSeqLen
    
    /** Generate tokens with KV cache - EFFICIENT version using single runUnsafe.
      * 
      * Uses GenerationLayout pattern with separate prefill/decode buffers.
      * Weights are uploaded ONCE, KV cache stays on GPU, only small I/O per token.
      */
    def generate(
      promptTokens: Array[Int],
      maxNewTokens: Int,
      sampleFn: Array[Float] => Int,
      onToken: Int => Unit = _ => (),
      stopTokens: Set[Int] = Set.empty,
      reportStats: Boolean = false,
    ): Array[Int] =
      require(promptTokens.length + maxNewTokens <= maxSeqLen, 
        s"Total sequence ${promptTokens.length + maxNewTokens} exceeds maxSeqLen=$maxSeqLen")
      
      currentSeqLen = 0
      val generatedTokens = scala.collection.mutable.ArrayBuffer[Int]()
      val prefillT = promptTokens.length
      
      // Pre-allocate I/O buffers
      val prefillTokensBuf = allocateIntBuffer(B * prefillT)
      prefillTokensBuf.asIntBuffer().put(promptTokens)
      prefillTokensBuf.rewind()
      val prefillLogitsBuf = allocateF32Buffer(B * prefillT * V)
      
      val decodeTokenBuf = allocateIntBuffer(B * 1)
      val decodeLogitsBuf = allocateF32Buffer(B * 1 * V)
      
      // Build pipelines
      val prefillPipeline = getOrBuildPipeline(prefillT, prefillT)
      val prefillParams = PipelineParams(config, B, prefillT, 0)
      
      // Build region with GenerationLayout - prefill first, then decode steps
      var region = GBufferRegion
        .allocate[GenerationLayout]
        .map: layout =>
          prefillPipeline.execute(prefillParams, layout.toPrefillLayout)
          layout
        .map: layout =>
          layout.prefillLogits.read(prefillLogitsBuf)
          prefillLogitsBuf.rewind()
          
          val logitsArr = new Array[Float](prefillT * V)
          copyFromF32Buffer(prefillLogitsBuf, logitsArr)
          val lastPosLogits = logitsArr.slice((prefillT - 1) * V, prefillT * V)
          val firstToken = sampleFn(lastPosLogits)
          generatedTokens += firstToken
          onToken(firstToken)
          currentSeqLen = prefillT
          
          if !stopTokens.contains(firstToken) && firstToken != 2 then
            decodeTokenBuf.clear()
            decodeTokenBuf.asIntBuffer().put(Array(firstToken))
            decodeTokenBuf.rewind()
            layout.decodeToken.write(decodeTokenBuf)
          
          layout
      
      var shouldStop = false
      val decodePipeline = getOrBuildPipeline(1, maxSeqLen)
      val attnParamsBuf = java.nio.ByteBuffer.allocateDirect(8).order(java.nio.ByteOrder.nativeOrder())
      
      region = (0 until maxNewTokens - 1).foldLeft(region): (regionAcc, step) =>
        val seqLen = prefillT + step + 1
        val startPos = seqLen - 1
        val decodeParams = PipelineParams(config, B, 1, startPos)
        
        regionAcc
          .map: layout =>
            if !shouldStop then
              attnParamsBuf.clear()
              attnParamsBuf.putInt(seqLen)
              attnParamsBuf.putInt(startPos)
              attnParamsBuf.flip()
              val uniform: io.computenode.cyfra.dsl.binding.GBinding[?] = layout.decodeAttnParams
              uniform.write(attnParamsBuf, 0)
              decodePipeline.execute(decodeParams, layout.toDecodeLayout)
            layout
          .map: layout =>
            if shouldStop then 
              layout
            else
              layout.decodeLogits.read(decodeLogitsBuf)
              decodeLogitsBuf.rewind()
              
              val logitsArr = new Array[Float](V)
              copyFromF32Buffer(decodeLogitsBuf, logitsArr)
              val nextToken = sampleFn(logitsArr)
              
              generatedTokens += nextToken
              onToken(nextToken)
              currentSeqLen += 1
              
              if stopTokens.contains(nextToken) || nextToken == 2 then
                shouldStop = true
              else
                decodeTokenBuf.clear()
                decodeTokenBuf.asIntBuffer().put(Array(nextToken))
                decodeTokenBuf.rewind()
                layout.decodeToken.write(decodeTokenBuf)
              
              layout
      
      val gPrefillParams = LlamaParams.create(config, prefillT, B, 0)
      val gDecodeParams = LlamaParams.create(config, 1, B, 0)
      
      def createAttnParamsBuffer(seqLen: Int, startPos: Int): ByteBuffer =
        val buf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())
        buf.putInt(seqLen)
        buf.putInt(startPos)
        buf.flip()
        buf
      
      val prefillAttnBuf = createAttnParamsBuffer(prefillT, 0)
      val decodeAttnBuf = createAttnParamsBuffer(1, 0)
      
      region.runUnsafe(
        init = GenerationLayout(
          tokenEmbed = GBuffer[Float32](tokenEmbedBuf),
          attnNorm = GBuffer[Float32](attnNormBuf),
          ffnNorm = GBuffer[Float32](ffnNormBuf),
          outputNorm = GBuffer[Float32](outputNormBuf),
          outputWeight = GBuffer[Float32](outputWeightBuf),
          wq = GBuffer[UInt32](wqBuf),
          wk = GBuffer[UInt32](wkBuf),
          wo = GBuffer[UInt32](woBuf),
          ffnGate = GBuffer[UInt32](ffnGateBuf),
          ffnUp = GBuffer[UInt32](ffnUpBuf),
          ffnDown = GBuffer[UInt32](ffnDownBuf),
          wv = GBuffer[UInt32](wvBuf),
          kCache = GBuffer[Float32](L * maxSeqLen * kvSize),
          vCache = GBuffer[Float32](L * maxSeqLen * kvSize),
          prefillTokens = GBuffer[Int32](prefillTokensBuf),
          prefillHidden = GBuffer[Float32](B * prefillT * C),
          prefillResidual = GBuffer[Float32](B * prefillT * C),
          prefillAttnNormOut = GBuffer[Float32](B * prefillT * C),
          prefillQ = GBuffer[Float32](B * prefillT * C),
          prefillK = GBuffer[Float32](B * prefillT * kvSize),
          prefillV = GBuffer[Float32](B * prefillT * kvSize),
          prefillQRoped = GBuffer[Float32](B * prefillT * C),
          prefillKRoped = GBuffer[Float32](B * prefillT * kvSize),
          prefillAttnOut = GBuffer[Float32](B * prefillT * C),
          prefillFfnNormOut = GBuffer[Float32](B * prefillT * C),
          prefillGate = GBuffer[Float32](B * prefillT * FFN),
          prefillUp = GBuffer[Float32](B * prefillT * FFN),
          prefillFfnHidden = GBuffer[Float32](B * prefillT * FFN),
          prefillFfnOut = GBuffer[Float32](B * prefillT * C),
          prefillLogits = GBuffer[Float32](prefillLogitsBuf),
          prefillParams = GUniform(gPrefillParams),
          prefillAttnParams = GUniform[AttentionParams](prefillAttnBuf),
          decodeToken = GBuffer[Int32](decodeTokenBuf),
          decodeHidden = GBuffer[Float32](B * 1 * C),
          decodeResidual = GBuffer[Float32](B * 1 * C),
          decodeAttnNormOut = GBuffer[Float32](B * 1 * C),
          decodeQ = GBuffer[Float32](B * 1 * C),
          decodeK = GBuffer[Float32](B * 1 * kvSize),
          decodeV = GBuffer[Float32](B * 1 * kvSize),
          decodeQRoped = GBuffer[Float32](B * 1 * C),
          decodeKRoped = GBuffer[Float32](B * 1 * kvSize),
          decodeAttnOut = GBuffer[Float32](B * 1 * C),
          decodeFfnNormOut = GBuffer[Float32](B * 1 * C),
          decodeGate = GBuffer[Float32](B * 1 * FFN),
          decodeUp = GBuffer[Float32](B * 1 * FFN),
          decodeFfnHidden = GBuffer[Float32](B * 1 * FFN),
          decodeFfnOut = GBuffer[Float32](B * 1 * C),
          decodeLogits = GBuffer[Float32](decodeLogitsBuf),
          decodeParams = GUniform(gDecodeParams),
          decodeAttnParams = GUniform[AttentionParams](decodeAttnBuf),
        ),
        onDone = _ => (),
      )
      
      // Basic stats (F32 pipeline doesn't have fine-grained timing)
      if reportStats then
        _lastStats = GenerationStats(
          promptTokens = promptTokens.length,
          generatedTokens = generatedTokens.length,
          prefillTimeMs = 0,
          decodeTimeMs = 0,
          totalTimeMs = 0,
        )
        Logger.info(_lastStats.toString)
      
      generatedTokens.toArray
    
    /** Legacy API: Prefill (for backward compatibility with tests). */
    def prefill(tokens: Array[Int]): Array[Float] =
      require(tokens.length <= maxSeqLen, s"Prompt length ${tokens.length} exceeds maxSeqLen=$maxSeqLen")
      currentSeqLen = 0
      val logits = forwardWithKVCache(tokens, startPos = 0)
      currentSeqLen = tokens.length
      val lastPosLogits = new Array[Float](V)
      System.arraycopy(logits, (tokens.length - 1) * V, lastPosLogits, 0, V)
      lastPosLogits
    
    /** Legacy API: Decode single token (for backward compatibility with tests). */
    def decode(token: Int): Array[Float] =
      require(currentSeqLen < maxSeqLen, s"Sequence length reached maxSeqLen=$maxSeqLen")
      val logits = forwardWithKVCache(Array(token), startPos = currentSeqLen)
      currentSeqLen += 1
      logits
    
    // Build KV-cached pipeline for given T and seqLen
    private def buildKVCachedPipeline(T: Int, seqLen: Int): GExecution[PipelineParams, KVCachePipelineLayout, KVCachePipelineLayout] =
      val eps = config.rmsNormEps.toFloat
      val theta = config.ropeTheta.toFloat
      val startPos = seqLen - T
      
      val embSizes = EmbeddingProgram.Sizes(B * T, C, V)
      var pipeline = GExecution[PipelineParams, KVCachePipelineLayout]()
        .addProgram(EmbeddingProgram.forward(embSizes))(
          _ => embSizes,
          l => EmbeddingProgram.ProgramLayout(l.tokens, l.tokenEmbed, l.hidden),
        )
      
      for layer <- 0 until L do
        val normOffset = layer * C
        val wqOffset = layer * wqUint32PerLayer
        val wkOffset = layer * wkUint32PerLayer
        val woOffset = layer * woUint32PerLayer
        val ffnGateOffset = layer * ffnGateUint32PerLayer
        val ffnUpOffset = layer * ffnUpUint32PerLayer
        
        val vOffsetUint32 = vOffsets(layer)
        val vIsQ6K = vTypes(layer) == Q6K
        val downOffsetUint32 = downOffsets(layer)
        val downIsQ6K = downTypes(layer) == Q6K
        
        val kvCacheLayerOffset = layer * maxSeqLen * kvSize
        
        val copySizes = CopyProgram.Sizes(B * T * C)
        val attnNormSizes = RMSNormProgram.Sizes(B * T, C, eps, normOffset, L * C)
        val qSizes = Q4KMatmulLayered.Sizes(B * T, C, C, wqOffset, L * wqUint32PerLayer)
        val kSizes = Q4KMatmulLayered.Sizes(B * T, C, kvSize, wkOffset, L * wkUint32PerLayer)
        val ropeQSizes = RoPEProgram.Sizes(B, T, NH, headSize, theta, startPos)
        val ropeKSizes = RoPEProgram.Sizes(B, T, NKV, headSize, theta, startPos)
        val woSizes = Q4KMatmulLayered.Sizes(B * T, C, C, woOffset, L * woUint32PerLayer)
        val resSizes = ResidualAddProgram.Sizes(B * T * C)
        val ffnNormSizes = RMSNormProgram.Sizes(B * T, C, eps, normOffset, L * C)
        val gateSizes = Q4KMatmulLayered.Sizes(B * T, C, FFN, ffnGateOffset, L * ffnGateUint32PerLayer)
        val upSizes = Q4KMatmulLayered.Sizes(B * T, C, FFN, ffnUpOffset, L * ffnUpUint32PerLayer)
        val swiGluSizes = SwiGLUProgram.Sizes(B * T * FFN)
        
        pipeline = pipeline.addProgram(CopyProgram.forward(copySizes))(
          _ => copySizes,
          l => CopyProgram.ProgramLayout(l.hidden, l.residual),
        )
        
        pipeline = pipeline.addProgram(RMSNormProgram.forward(attnNormSizes))(
          _ => attnNormSizes,
          l => RMSNormProgram.ProgramLayout(l.hidden, l.attnNorm, l.attnNormOut),
        )
        
        pipeline = pipeline
          .addProgram(Q4KMatmulLayered.forward(qSizes))(
            _ => qSizes,
            l => Q4KMatmulLayered.ProgramLayout(l.wq, l.attnNormOut, l.q),
          )
          .addProgram(Q4KMatmulLayered.forward(kSizes))(
            _ => kSizes,
            l => Q4KMatmulLayered.ProgramLayout(l.wk, l.attnNormOut, l.k),
          )
        
        if vIsQ6K then
          val vQ6kOffset = vOffsetUint32 * 4
          val vSizes = Q6KMatmulLayered.Sizes(B * T, C, kvSize, vQ6kOffset, totalVUint32 * 4)
          pipeline = pipeline.addProgram(Q6KMatmulLayered.forward(vSizes))(
            _ => vSizes,
            l => Q6KMatmulLayered.ProgramLayout(l.wv, l.attnNormOut, l.v),
          )
        else
          val vSizes = Q4KMatmulLayered.Sizes(B * T, C, kvSize, vOffsetUint32, totalVUint32)
          pipeline = pipeline.addProgram(Q4KMatmulLayered.forward(vSizes))(
            _ => vSizes,
            l => Q4KMatmulLayered.ProgramLayout(l.wv, l.attnNormOut, l.v),
          )
        
        pipeline = pipeline
          .addProgram(RoPEProgram.forward(ropeQSizes))(
            _ => ropeQSizes,
            l => RoPEProgram.ProgramLayout(l.q, l.qRoped, l.attnParams),
          )
          .addProgram(RoPEProgram.forward(ropeKSizes))(
            _ => ropeKSizes,
            l => RoPEProgram.ProgramLayout(l.k, l.kRoped, l.attnParams),
          )
        
        val kvWriteKSizes = KVCacheWriteK.Sizes(B, T, NKV, headSize, maxSeqLen, layer, startPos, kvCacheLayerOffset, L)
        val kvWriteVSizes = KVCacheWriteV.Sizes(B, T, NKV, headSize, maxSeqLen, layer, startPos, kvCacheLayerOffset, L)
        
        pipeline = pipeline
          .addProgram(KVCacheWriteK.forward(kvWriteKSizes))(
            _ => kvWriteKSizes,
            l => KVCacheWriteK.ProgramLayout(l.kRoped, l.kCache, l.attnParams),
          )
          .addProgram(KVCacheWriteV.forward(kvWriteVSizes))(
            _ => kvWriteVSizes,
            l => KVCacheWriteV.ProgramLayout(l.v, l.vCache, l.attnParams),
          )
        
        val attnSizes = KVCachedAttention.Sizes(B, T, NH, NKV, headSize, startPos, kvCacheLayerOffset, kvCacheLayerOffset, L, maxSeqLen)
        pipeline = pipeline.addProgram(KVCachedAttention.forward(attnSizes))(
          _ => attnSizes,
          l => KVCachedAttention.ProgramLayout(l.qRoped, l.kCache, l.vCache, l.attnOut, l.attnParams),
        )
        
        pipeline = pipeline
          .addProgram(Q4KMatmulLayered.forward(woSizes))(
            _ => woSizes,
            l => Q4KMatmulLayered.ProgramLayout(l.wo, l.attnOut, l.hidden),
          )
          .addProgram(ResidualAddProgram.forward(resSizes))(
            _ => resSizes,
            l => ResidualAddProgram.ProgramLayout(l.residual, l.hidden, l.attnNormOut),
          )
        
        pipeline = pipeline
          .addProgram(CopyProgram.forward(copySizes))(
            _ => copySizes,
            l => CopyProgram.ProgramLayout(l.attnNormOut, l.residual),
          )
          .addProgram(RMSNormProgram.forward(ffnNormSizes))(
            _ => ffnNormSizes,
            l => RMSNormProgram.ProgramLayout(l.attnNormOut, l.ffnNorm, l.ffnNormOut),
          )
          .addProgram(Q4KMatmulLayered.forward(gateSizes))(
            _ => gateSizes,
            l => Q4KMatmulLayered.ProgramLayout(l.ffnGate, l.ffnNormOut, l.gate),
          )
          .addProgram(Q4KMatmulLayered.forward(upSizes))(
            _ => upSizes,
            l => Q4KMatmulLayered.ProgramLayout(l.ffnUp, l.ffnNormOut, l.up),
          )
          .addProgram(SwiGLUProgram.forward(swiGluSizes))(
            _ => swiGluSizes,
            l => SwiGLUProgram.ProgramLayout(l.gate, l.up, l.ffnHidden),
          )
        
        if downIsQ6K then
          val downQ6kOffset = downOffsetUint32 * 4
          val downSizes = Q6KMatmulLayered.Sizes(B * T, FFN, C, downQ6kOffset, totalDownUint32 * 4)
          pipeline = pipeline.addProgram(Q6KMatmulLayered.forward(downSizes))(
            _ => downSizes,
            l => Q6KMatmulLayered.ProgramLayout(l.ffnDown, l.ffnHidden, l.ffnOut),
          )
        else
          val downSizes = Q4KMatmulLayered.Sizes(B * T, FFN, C, downOffsetUint32, totalDownUint32)
          pipeline = pipeline.addProgram(Q4KMatmulLayered.forward(downSizes))(
            _ => downSizes,
            l => Q4KMatmulLayered.ProgramLayout(l.ffnDown, l.ffnHidden, l.ffnOut),
          )
        
        pipeline = pipeline.addProgram(ResidualAddProgram.forward(resSizes))(
          _ => resSizes,
          l => ResidualAddProgram.ProgramLayout(l.residual, l.ffnOut, l.hidden),
        )
      end for
      
      val finalNormSizes = RMSNormProgram.Sizes(B * T, C, eps, 0, C)
      val logitsSizes = TiledMatmulVecProgram.Sizes(B * T, C, V, 0, V * C)
      
      pipeline
        .addProgram(RMSNormProgram.forward(finalNormSizes))(
          _ => finalNormSizes,
          l => RMSNormProgram.ProgramLayout(l.hidden, l.outputNorm, l.attnNormOut),
        )
        .addProgram(TiledMatmulVecProgram.forward(logitsSizes))(
          _ => logitsSizes,
          l => TiledMatmulVecProgram.ProgramLayout(l.outputWeight, l.attnNormOut, l.logits),
        )
    end buildKVCachedPipeline
    
    // Legacy KV cache buffers for prefill/decode API
    private lazy val legacyKCacheBuf = allocateF32Buffer(L * maxSeqLen * kvSize)
    private lazy val legacyVCacheBuf = allocateF32Buffer(L * maxSeqLen * kvSize)
    
    private def forwardWithKVCache(tokens: Array[Int], startPos: Int): Array[Float] =
      val T = tokens.length
      val seqLen = startPos + T
      
      tokenEmbedBuf.rewind()
      attnNormBuf.rewind()
      ffnNormBuf.rewind()
      outputNormBuf.rewind()
      outputWeightBuf.rewind()
      wqBuf.rewind()
      wkBuf.rewind()
      woBuf.rewind()
      ffnGateBuf.rewind()
      ffnUpBuf.rewind()
      wvBuf.rewind()
      ffnDownBuf.rewind()
      legacyKCacheBuf.rewind()
      legacyVCacheBuf.rewind()
      
      val tokensBuf = allocateIntBuffer(B * T)
      tokensBuf.asIntBuffer().put(tokens)
      tokensBuf.rewind()
      
      val pipeline = getOrBuildPipeline(T, seqLen)
      
      val gParams = LlamaParams.create(config, T, B, startPos)
      val pParams = PipelineParams(config, B, T, startPos)
      
      val logits = new Array[Float](B * T * V)
      val logitsBuf = allocateF32Buffer(B * T * V)
      
      val attnStartPos = seqLen - T
      val attnParamsBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())
      attnParamsBuf.putInt(seqLen)
      attnParamsBuf.putInt(attnStartPos)
      attnParamsBuf.flip()
      
      val region = GBufferRegion
        .allocate[KVCachePipelineLayout]
        .map(layout => pipeline.execute(pParams, layout))
      
      region.runUnsafe(
        init = KVCachePipelineLayout(
          tokens = GBuffer[Int32](tokensBuf),
          tokenEmbed = GBuffer[Float32](tokenEmbedBuf),
          attnNorm = GBuffer[Float32](attnNormBuf),
          ffnNorm = GBuffer[Float32](ffnNormBuf),
          outputNorm = GBuffer[Float32](outputNormBuf),
          outputWeight = GBuffer[Float32](outputWeightBuf),
          wq = GBuffer[UInt32](wqBuf),
          wk = GBuffer[UInt32](wkBuf),
          wo = GBuffer[UInt32](woBuf),
          ffnGate = GBuffer[UInt32](ffnGateBuf),
          ffnUp = GBuffer[UInt32](ffnUpBuf),
          ffnDown = GBuffer[UInt32](ffnDownBuf),
          wv = GBuffer[UInt32](wvBuf),
          kCache = GBuffer[Float32](legacyKCacheBuf),
          vCache = GBuffer[Float32](legacyVCacheBuf),
          hidden = GBuffer[Float32](B * T * C),
          residual = GBuffer[Float32](B * T * C),
          attnNormOut = GBuffer[Float32](B * T * C),
          q = GBuffer[Float32](B * T * C),
          k = GBuffer[Float32](B * T * kvSize),
          v = GBuffer[Float32](B * T * kvSize),
          qRoped = GBuffer[Float32](B * T * C),
          kRoped = GBuffer[Float32](B * T * kvSize),
          attnOut = GBuffer[Float32](B * T * C),
          ffnNormOut = GBuffer[Float32](B * T * C),
          gate = GBuffer[Float32](B * T * FFN),
          up = GBuffer[Float32](B * T * FFN),
          ffnHidden = GBuffer[Float32](B * T * FFN),
          ffnOut = GBuffer[Float32](B * T * C),
          logits = GBuffer[Float32](logitsBuf),
          params = GUniform(gParams),
          attnParams = GUniform[AttentionParams](attnParamsBuf),
        ),
        onDone = layout =>
          layout.kCache.read(legacyKCacheBuf)
          layout.vCache.read(legacyVCacheBuf)
          layout.logits.read(logitsBuf)
          copyFromF32Buffer(logitsBuf, logits),
      )
      
      logits
    end forwardWithKVCache
    /** Last generation statistics. */
    private var _lastStats: GenerationStats = null
    override def lastStats: Option[GenerationStats] = Option(_lastStats)
    
  end F32KVCachedPipeline

end LlamaF32Pipeline
