package io.computenode.cyfra.llama.pipeline

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GCodec, GExecution, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.Value.FromExpr
import io.computenode.cyfra.dsl.struct.GStruct.Empty
import io.computenode.cyfra.dsl.struct.GStructSchema
import io.computenode.cyfra.dsl.given_GStructConstructor_T
import io.computenode.cyfra.llama.model.LlamaConfig
import io.computenode.cyfra.llama.pipeline.PipelineUtils.*
import io.computenode.cyfra.llama.programs.*
import io.computenode.cyfra.llama.programs.f16.*
import io.computenode.cyfra.llama.util.Logger

import java.nio.{ByteBuffer, ByteOrder}

/** F16-Native Llama GPU Pipeline - all compute in half precision.
  *
  * This pipeline keeps everything in F16 for maximum memory efficiency:
  *   - F16 weights loaded directly from GGUF (no conversion)
  *   - F16 compute throughout (matmul, attention, FFN)
  *   - Only final logits in F32 (for softmax stability)
  *   - 2x memory savings vs F32 pipeline
  *
  * Follows the same optimized pattern as LlamaF32Pipeline:
  *   - Single GExecution covering ALL operations
  *   - All layer parameters concatenated in single buffers
  *   - Layer offsets computed at compile time per-program
  *   - ByteBuffer-based I/O for efficiency
  *   - Only one runUnsafe at the end
  */
object LlamaF16Pipeline:

  // ============= F16 Pipeline Params =============

  case class F16PipelineParams(
    config: LlamaConfig,
    B: Int,
    T: Int,
    startPos: Int = 0,
  ):
    def C: Int = config.hiddenSize
    def NH: Int = config.numAttentionHeads
    def NKV: Int = config.numKeyValueHeads
    def headSize: Int = config.headSize
    def FFN: Int = config.intermediateSize
    def V: Int = config.vocabSize
    def L: Int = config.numHiddenLayers
    def kvSize: Int = NKV * headSize

  // ============= F16 Model Weights =============
  
  /** F16 weights structure - keeps F16 bytes, no F32 conversion */
  case class F16LayerWeights(
    attnNorm: Array[Byte],    // F16 bytes
    wq: Array[Byte],          // F16 bytes
    wk: Array[Byte],          // F16 bytes
    wv: Array[Byte],          // F16 bytes
    wo: Array[Byte],          // F16 bytes
    ffnNorm: Array[Byte],     // F16 bytes
    ffnGate: Array[Byte],     // F16 bytes
    ffnUp: Array[Byte],       // F16 bytes
    ffnDown: Array[Byte],     // F16 bytes
  )
  
  case class F16ModelWeights(
    tokenEmbed: Array[Byte],   // F16 bytes
    layers: Seq[F16LayerWeights],
    outputNorm: Array[Byte],   // F16 bytes
    output: Array[Byte],       // F16 bytes (or same as tokenEmbed for tied)
  )

  // ============= F16 KV Cache Pipeline Layout (Vec4 Optimized) =============
  
  /** F16 KV Cache Pipeline layout with Vec4 weights for 4x memory bandwidth.
    * 
    * Layout structure follows LlamaF32Pipeline.KVCachePipelineLayout:
    *   - kCache/vCache: (L, maxSeqLen, NKV, headSize) - persistent across forward calls
    *   - Other buffers: sized for current T (can be 1 for decode)
    */
  case class F16KVCachePipelineLayout(
    // Input
    tokens: GBuffer[Int32],
    
    // Scalar weights (embedding lookup, norms need scalar indexing)
    tokenEmbed: GBuffer[Float16],   // (V, C) - scalar for embedding lookup
    attnNorm: GBuffer[Float16],     // (L, C) - scalar for RMSNorm
    ffnNorm: GBuffer[Float16],      // (L, C) - scalar for RMSNorm
    outputNorm: GBuffer[Float16],   // (C) - scalar for RMSNorm
    
    // Vec4 weights (matmul - 4x bandwidth!)
    wq: GBuffer[Vec4[Float16]],           // (L, C, C/4)
    wk: GBuffer[Vec4[Float16]],           // (L, C, kvSize/4)
    wv: GBuffer[Vec4[Float16]],           // (L, C, kvSize/4)
    wo: GBuffer[Vec4[Float16]],           // (L, C, C/4)
    ffnGate: GBuffer[Vec4[Float16]],      // (L, FFN, C/4)
    ffnUp: GBuffer[Vec4[Float16]],        // (L, FFN, C/4)
    ffnDown: GBuffer[Vec4[Float16]],      // (L, C, FFN/4)
    outputWeight: GBuffer[Vec4[Float16]], // (V, C/4)
    
    // KV Cache - F16
    kCache: GBuffer[Float16],
    vCache: GBuffer[Float16],
    
    // Activations - scalar F16 (shared across programs)
    hidden: GBuffer[Float16],
    residual: GBuffer[Float16],
    attnNormOut: GBuffer[Float16],
    q: GBuffer[Float16],
    k: GBuffer[Float16],
    v: GBuffer[Float16],
    qRoped: GBuffer[Float16],
    kRoped: GBuffer[Float16],
    attnOut: GBuffer[Float16],
    ffnNormOut: GBuffer[Float16],
    gate: GBuffer[Float16],
    up: GBuffer[Float16],
    ffnHidden: GBuffer[Float16],
    ffnOut: GBuffer[Float16],
    logits: GBuffer[Float32],
    
    attnParams: GUniform[AttentionParams],
  ) derives Layout

  /** F16 Combined layout for generation with Vec4 optimized weights. */
  case class F16GenerationLayout(
    // === Scalar weights ===
    tokenEmbed: GBuffer[Float16],
    attnNorm: GBuffer[Float16],
    ffnNorm: GBuffer[Float16],
    outputNorm: GBuffer[Float16],
    
    // === Vec4 weights (4x bandwidth!) ===
    wq: GBuffer[Vec4[Float16]],
    wk: GBuffer[Vec4[Float16]],
    wv: GBuffer[Vec4[Float16]],
    wo: GBuffer[Vec4[Float16]],
    ffnGate: GBuffer[Vec4[Float16]],
    ffnUp: GBuffer[Vec4[Float16]],
    ffnDown: GBuffer[Vec4[Float16]],
    outputWeight: GBuffer[Vec4[Float16]],
    
    // === KV Cache ===
    kCache: GBuffer[Float16],
    vCache: GBuffer[Float16],
    
    // === Prefill I/O ===
    prefillTokens: GBuffer[Int32],
    prefillHidden: GBuffer[Float16],
    prefillResidual: GBuffer[Float16],
    prefillAttnNormOut: GBuffer[Float16],
    prefillQ: GBuffer[Float16],
    prefillK: GBuffer[Float16],
    prefillV: GBuffer[Float16],
    prefillQRoped: GBuffer[Float16],
    prefillKRoped: GBuffer[Float16],
    prefillAttnOut: GBuffer[Float16],
    prefillFfnNormOut: GBuffer[Float16],
    prefillGate: GBuffer[Float16],
    prefillUp: GBuffer[Float16],
    prefillFfnHidden: GBuffer[Float16],
    prefillFfnOut: GBuffer[Float16],
    prefillLogits: GBuffer[Float32],
    prefillAttnParams: GUniform[AttentionParams],
    
    // === Decode I/O ===
    decodeToken: GBuffer[Int32],
    decodeHidden: GBuffer[Float16],
    decodeResidual: GBuffer[Float16],
    decodeAttnNormOut: GBuffer[Float16],
    decodeQ: GBuffer[Float16],
    decodeK: GBuffer[Float16],
    decodeV: GBuffer[Float16],
    decodeQRoped: GBuffer[Float16],
    decodeKRoped: GBuffer[Float16],
    decodeAttnOut: GBuffer[Float16],
    decodeFfnNormOut: GBuffer[Float16],
    decodeGate: GBuffer[Float16],
    decodeUp: GBuffer[Float16],
    decodeFfnHidden: GBuffer[Float16],
    decodeFfnOut: GBuffer[Float16],
    decodeLogits: GBuffer[Float32],
    decodeAttnParams: GUniform[AttentionParams],
  ) derives Layout:
    def toPrefillLayout: F16KVCachePipelineLayout = F16KVCachePipelineLayout(
      tokens = prefillTokens,
      tokenEmbed = tokenEmbed, attnNorm = attnNorm, ffnNorm = ffnNorm, outputNorm = outputNorm,
      wq = wq, wk = wk, wv = wv, wo = wo,
      ffnGate = ffnGate, ffnUp = ffnUp, ffnDown = ffnDown, outputWeight = outputWeight,
      kCache = kCache, vCache = vCache,
      hidden = prefillHidden, residual = prefillResidual, attnNormOut = prefillAttnNormOut,
      q = prefillQ, k = prefillK, v = prefillV, qRoped = prefillQRoped, kRoped = prefillKRoped,
      attnOut = prefillAttnOut, ffnNormOut = prefillFfnNormOut,
      gate = prefillGate, up = prefillUp, ffnHidden = prefillFfnHidden, ffnOut = prefillFfnOut,
      logits = prefillLogits, attnParams = prefillAttnParams,
    )
    
    def toDecodeLayout: F16KVCachePipelineLayout = F16KVCachePipelineLayout(
      tokens = decodeToken,
      tokenEmbed = tokenEmbed, attnNorm = attnNorm, ffnNorm = ffnNorm, outputNorm = outputNorm,
      wq = wq, wk = wk, wv = wv, wo = wo,
      ffnGate = ffnGate, ffnUp = ffnUp, ffnDown = ffnDown, outputWeight = outputWeight,
      kCache = kCache, vCache = vCache,
      hidden = decodeHidden, residual = decodeResidual, attnNormOut = decodeAttnNormOut,
      q = decodeQ, k = decodeK, v = decodeV, qRoped = decodeQRoped, kRoped = decodeKRoped,
      attnOut = decodeAttnOut, ffnNormOut = decodeFfnNormOut,
      gate = decodeGate, up = decodeUp, ffnHidden = decodeFfnHidden, ffnOut = decodeFfnOut,
      logits = decodeLogits, attnParams = decodeAttnParams,
    )


  // ============= F16 KV Cached Pipeline Build =============
  
  /** Build F16 KV-cached pipeline with Vec4 optimized matmuls.
    * 
    * Uses F16MatmulVecHybridProgram for 4x weight memory bandwidth.
    */
  def buildF16KVCachedPipeline(
    config: LlamaConfig,
    B: Int,
    T: Int,
    maxSeqLen: Int,
  ): GExecution[F16PipelineParams, F16KVCachePipelineLayout, F16KVCachePipelineLayout] =
    val C = config.hiddenSize
    val NH = config.numAttentionHeads
    val NKV = config.numKeyValueHeads
    val headSize = config.headSize
    val FFN = config.intermediateSize
    val V = config.vocabSize
    val L = config.numHiddenLayers
    val kvSize = NKV * headSize
    val eps = config.rmsNormEps.toFloat
    val theta = config.ropeTheta.toFloat
    val startPos = maxSeqLen - T
    
    // Verify dimensions are divisible by 4 for Vec4 optimization
    require(C % 4 == 0, s"hiddenSize ($C) must be divisible by 4")
    require(kvSize % 4 == 0, s"kvSize ($kvSize) must be divisible by 4")
    require(FFN % 4 == 0, s"intermediateSize ($FFN) must be divisible by 4")

    // Embedding
    val embSizes = F16EmbeddingProgram.Sizes(B * T, C, V)
    var pipeline = GExecution[F16PipelineParams, F16KVCachePipelineLayout]()
      .addProgram(F16EmbeddingProgram.forward(embSizes))(
        _ => embSizes,
        l => F16EmbeddingProgram.ProgramLayout(l.tokens, l.tokenEmbed, l.hidden),
      )

    // Process each layer with Vec4 optimized matmuls
    for layer <- 0 until L do
      val normOffset = layer * C
      val ffnNormOffset = layer * C
      
      // Vec4 weight offsets (in Vec4 units = elements / 4)
      val wqOffsetVec4 = layer * C * (C / 4)
      val wkOffsetVec4 = layer * C * (kvSize / 4)
      val wvOffsetVec4 = layer * C * (kvSize / 4)
      val woOffsetVec4 = layer * C * (C / 4)
      val ffnGateOffsetVec4 = layer * FFN * (C / 4)
      val ffnUpOffsetVec4 = layer * FFN * (C / 4)
      val ffnDownOffsetVec4 = layer * C * (FFN / 4)

      val kvCacheLayerOffset = layer * maxSeqLen * kvSize

      val copySizes = F16CopyProgram.Sizes(B * T * C)
      val attnNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, normOffset, L * C)
      
      // Vec4 matmul sizes
      val qSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, C, wqOffsetVec4, L * C * (C / 4))
      val kSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, kvSize, wkOffsetVec4, L * C * (kvSize / 4))
      val vSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, kvSize, wvOffsetVec4, L * C * (kvSize / 4))
      val woSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, C, woOffsetVec4, L * C * (C / 4))
      
      val ropeQSizes = F16RoPEProgram.Sizes(B, T, NH, headSize, theta)
      val ropeKSizes = F16RoPEProgram.Sizes(B, T, NKV, headSize, theta)
      val resSizes = F16ResidualAddProgram.Sizes(B * T * C)
      val ffnNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, ffnNormOffset, L * C)
      
      // FFN Vec4 matmul sizes
      val gateSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, FFN, ffnGateOffsetVec4, L * FFN * (C / 4))
      val upSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, FFN, ffnUpOffsetVec4, L * FFN * (C / 4))
      val swiGluSizes = F16SwiGLUProgram.Sizes(B * T * FFN)
      val downSizes = F16MatmulVecHybridProgram.Sizes(B * T, FFN, C, ffnDownOffsetVec4, L * C * (FFN / 4))

      // Save residual
      pipeline = pipeline.addProgram(F16CopyProgram.forward(copySizes))(
        _ => copySizes,
        l => F16CopyProgram.ProgramLayout(l.hidden, l.residual),
      )

      // Attention norm
      pipeline = pipeline.addProgram(F16RMSNormProgram.forward(attnNormSizes))(
        _ => attnNormSizes,
        l => F16RMSNormProgram.ProgramLayout(l.hidden, l.attnNorm, l.attnNormOut),
      )

      // Q, K, V projections with Vec4 weights
      pipeline = pipeline
        .addProgram(F16MatmulVecHybridProgram.forward(qSizes))(
          _ => qSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.wq, l.attnNormOut, l.q),
        )
        .addProgram(F16MatmulVecHybridProgram.forward(kSizes))(
          _ => kSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.wk, l.attnNormOut, l.k),
        )
        .addProgram(F16MatmulVecHybridProgram.forward(vSizes))(
          _ => vSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.wv, l.attnNormOut, l.v),
        )

      // RoPE
      pipeline = pipeline
        .addProgram(F16RoPEProgram.forward(ropeQSizes))(
          _ => ropeQSizes,
          l => F16RoPEProgram.ProgramLayout(l.q, l.qRoped, l.attnParams),
        )
        .addProgram(F16RoPEProgram.forward(ropeKSizes))(
          _ => ropeKSizes,
          l => F16RoPEProgram.ProgramLayout(l.k, l.kRoped, l.attnParams),
        )

      // KV Cache Write
      val kvWriteKSizes = F16KVCacheWriteK.Sizes(B, T, NKV, headSize, maxSeqLen, layer, startPos, kvCacheLayerOffset, L)
      val kvWriteVSizes = F16KVCacheWriteV.Sizes(B, T, NKV, headSize, maxSeqLen, layer, startPos, kvCacheLayerOffset, L)

      pipeline = pipeline
        .addProgram(F16KVCacheWriteK.forward(kvWriteKSizes))(
          _ => kvWriteKSizes,
          l => F16KVCacheWriteK.ProgramLayout(l.kRoped, l.kCache, l.attnParams),
        )
        .addProgram(F16KVCacheWriteV.forward(kvWriteVSizes))(
          _ => kvWriteVSizes,
          l => F16KVCacheWriteV.ProgramLayout(l.v, l.vCache, l.attnParams),
        )

      // Attention
      val attnSizes = F16KVCachedAttention.Sizes(B, T, NH, NKV, headSize, startPos, kvCacheLayerOffset, kvCacheLayerOffset, L, maxSeqLen)
      pipeline = pipeline.addProgram(F16KVCachedAttention.forward(attnSizes))(
        _ => attnSizes,
        l => F16KVCachedAttention.ProgramLayout(l.qRoped, l.kCache, l.vCache, l.attnOut, l.attnParams),
      )

      // Output projection with Vec4 weights
      pipeline = pipeline
        .addProgram(F16MatmulVecHybridProgram.forward(woSizes))(
          _ => woSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.wo, l.attnOut, l.hidden),
        )
        .addProgram(F16ResidualAddProgram.forward(resSizes))(
          _ => resSizes,
          l => F16ResidualAddProgram.ProgramLayout(l.residual, l.hidden, l.attnNormOut),
        )

      // FFN with Vec4 weights
      pipeline = pipeline
        .addProgram(F16CopyProgram.forward(copySizes))(
          _ => copySizes,
          l => F16CopyProgram.ProgramLayout(l.attnNormOut, l.residual),
        )
        .addProgram(F16RMSNormProgram.forward(ffnNormSizes))(
          _ => ffnNormSizes,
          l => F16RMSNormProgram.ProgramLayout(l.attnNormOut, l.ffnNorm, l.ffnNormOut),
        )
        .addProgram(F16MatmulVecHybridProgram.forward(gateSizes))(
          _ => gateSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.ffnGate, l.ffnNormOut, l.gate),
        )
        .addProgram(F16MatmulVecHybridProgram.forward(upSizes))(
          _ => upSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.ffnUp, l.ffnNormOut, l.up),
        )
        .addProgram(F16SwiGLUProgram.forward(swiGluSizes))(
          _ => swiGluSizes,
          l => F16SwiGLUProgram.ProgramLayout(l.gate, l.up, l.ffnHidden),
        )
        .addProgram(F16MatmulVecHybridProgram.forward(downSizes))(
          _ => downSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.ffnDown, l.ffnHidden, l.ffnOut),
        )
        .addProgram(F16ResidualAddProgram.forward(resSizes))(
          _ => resSizes,
          l => F16ResidualAddProgram.ProgramLayout(l.residual, l.ffnOut, l.hidden),
        )
    end for

    // Final norm and output projection with Vec4 weights
    val finalNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, 0, C)
    val logitsSizes = F16OutputVec4Program.Sizes(B * T, C, V)

    pipeline
      .addProgram(F16RMSNormProgram.forward(finalNormSizes))(
        _ => finalNormSizes,
        l => F16RMSNormProgram.ProgramLayout(l.hidden, l.outputNorm, l.attnNormOut),
      )
      .addProgram(F16OutputVec4Program.forward(logitsSizes))(
        _ => logitsSizes,
        l => F16OutputVec4Program.ProgramLayout(l.attnNormOut, l.outputWeight, l.logits),
      )
  end buildF16KVCachedPipeline

  // ============= F16 KV Cached Pipeline (FUSED) =============

  /** Build F16 KV-cached pipeline with FUSED kernels for reduced dispatch count.
    *
    * Fused operations:
    *   - Q/K/V projections: 3 dispatches → 1 (F16FusedQKVMatmulProgram)
    *   - RoPE Q/K: 2 dispatches → 1 (F16FusedRoPEProgram)
    *   - KV cache write: 2 dispatches → 1 (F16FusedKVCacheWriteProgram)
    *   - Gate/Up/SwiGLU: 3 dispatches → 1 (F16FusedGateUpSwiGLUProgram)
    *
    * Total savings: 6 dispatches per layer
    * For 16-layer model: 307 → 211 dispatches per token (31% reduction)
    */
  def buildF16KVCachedPipelineFused(
    config: LlamaConfig,
    B: Int,
    T: Int,
    maxSeqLen: Int,
  ): GExecution[F16PipelineParams, F16KVCachePipelineLayout, F16KVCachePipelineLayout] =
    val C = config.hiddenSize
    val NH = config.numAttentionHeads
    val NKV = config.numKeyValueHeads
    val headSize = config.headSize
    val FFN = config.intermediateSize
    val V = config.vocabSize
    val L = config.numHiddenLayers
    val kvSize = NKV * headSize
    val eps = config.rmsNormEps.toFloat
    val theta = config.ropeTheta.toFloat
    val startPos = maxSeqLen - T

    // Verify dimensions are divisible by 4 for Vec4 optimization
    require(C % 4 == 0, s"hiddenSize ($C) must be divisible by 4")
    require(kvSize % 4 == 0, s"kvSize ($kvSize) must be divisible by 4")
    require(FFN % 4 == 0, s"intermediateSize ($FFN) must be divisible by 4")

    // Embedding
    val embSizes = F16EmbeddingProgram.Sizes(B * T, C, V)
    var pipeline = GExecution[F16PipelineParams, F16KVCachePipelineLayout]()
      .addProgram(F16EmbeddingProgram.forward(embSizes))(
        _ => embSizes,
        l => F16EmbeddingProgram.ProgramLayout(l.tokens, l.tokenEmbed, l.hidden),
      )

    // Process each layer with FUSED kernels
    for layer <- 0 until L do
      val normOffset = layer * C
      val ffnNormOffset = layer * C

      // Vec4 weight offsets (in Vec4 units = elements / 4)
      val wqOffsetVec4 = layer * C * (C / 4)
      val wkOffsetVec4 = layer * C * (kvSize / 4)
      val wvOffsetVec4 = layer * C * (kvSize / 4)
      val woOffsetVec4 = layer * C * (C / 4)
      val ffnGateOffsetVec4 = layer * FFN * (C / 4)
      val ffnUpOffsetVec4 = layer * FFN * (C / 4)
      val ffnDownOffsetVec4 = layer * C * (FFN / 4)

      val kvCacheLayerOffset = layer * maxSeqLen * kvSize

      val copySizes = F16CopyProgram.Sizes(B * T * C)
      val attnNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, normOffset, L * C)

      // FUSED Q/K/V projection (3 → 1 dispatch)
      val qkvSizes = F16FusedQKVMatmulProgram.Sizes(
        batchSize = B * T,
        inFeatures = C,
        qOutFeatures = C,
        kvOutFeatures = kvSize,
        wqOffsetVec4 = wqOffsetVec4,
        wkOffsetVec4 = wkOffsetVec4,
        wvOffsetVec4 = wvOffsetVec4,
        totalWqVec4 = L * C * (C / 4),
        totalWkVec4 = L * C * (kvSize / 4),
        totalWvVec4 = L * C * (kvSize / 4),
      )

      // FUSED RoPE for Q and K (2 → 1 dispatch)
      val fusedRopeSizes = F16FusedRoPEProgram.Sizes(B, T, NH, NKV, headSize, theta)

      // FUSED KV cache write (2 → 1 dispatch)
      val fusedKVWriteSizes = F16FusedKVCacheWriteProgram.Sizes(
        B, T, NKV, headSize, maxSeqLen, layer, startPos, kvCacheLayerOffset, kvCacheLayerOffset, L,
      )

      val woSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, C, woOffsetVec4, L * C * (C / 4))
      val resSizes = F16ResidualAddProgram.Sizes(B * T * C)
      val ffnNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, ffnNormOffset, L * C)

      // FUSED Gate/Up/SwiGLU (3 → 1 dispatch)
      val fusedGateUpSizes = F16FusedGateUpSwiGLUProgram.Sizes(
        batchSize = B * T,
        inFeatures = C,
        outFeatures = FFN,
        gateOffsetVec4 = ffnGateOffsetVec4,
        upOffsetVec4 = ffnUpOffsetVec4,
        totalGateVec4 = L * FFN * (C / 4),
        totalUpVec4 = L * FFN * (C / 4),
      )

      val downSizes = F16MatmulVecHybridProgram.Sizes(B * T, FFN, C, ffnDownOffsetVec4, L * C * (FFN / 4))

      val attnSizes = F16KVCachedAttention.Sizes(B, T, NH, NKV, headSize, startPos, kvCacheLayerOffset, kvCacheLayerOffset, L, maxSeqLen)

      // Save residual
      pipeline = pipeline.addProgram(F16CopyProgram.forward(copySizes))(
        _ => copySizes,
        l => F16CopyProgram.ProgramLayout(l.hidden, l.residual),
      )

      // Attention norm
      pipeline = pipeline.addProgram(F16RMSNormProgram.forward(attnNormSizes))(
        _ => attnNormSizes,
        l => F16RMSNormProgram.ProgramLayout(l.hidden, l.attnNorm, l.attnNormOut),
      )

      // FUSED Q/K/V projections
      pipeline = pipeline.addProgram(F16FusedQKVMatmulProgram.forward(qkvSizes))(
        _ => qkvSizes,
        l => F16FusedQKVMatmulProgram.ProgramLayout(l.wq, l.wk, l.wv, l.attnNormOut, l.q, l.k, l.v),
      )

      // FUSED RoPE
      pipeline = pipeline.addProgram(F16FusedRoPEProgram.forward(fusedRopeSizes))(
        _ => fusedRopeSizes,
        l => F16FusedRoPEProgram.ProgramLayout(l.q, l.k, l.qRoped, l.kRoped, l.attnParams),
      )

      // FUSED KV Cache Write
      pipeline = pipeline.addProgram(F16FusedKVCacheWriteProgram.forward(fusedKVWriteSizes))(
        _ => fusedKVWriteSizes,
        l => F16FusedKVCacheWriteProgram.ProgramLayout(l.kRoped, l.v, l.kCache, l.vCache, l.attnParams),
      )

      // Attention
      pipeline = pipeline.addProgram(F16KVCachedAttention.forward(attnSizes))(
        _ => attnSizes,
        l => F16KVCachedAttention.ProgramLayout(l.qRoped, l.kCache, l.vCache, l.attnOut, l.attnParams),
      )

      // Output projection + residual
      pipeline = pipeline
        .addProgram(F16MatmulVecHybridProgram.forward(woSizes))(
          _ => woSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.wo, l.attnOut, l.hidden),
        )
        .addProgram(F16ResidualAddProgram.forward(resSizes))(
          _ => resSizes,
          l => F16ResidualAddProgram.ProgramLayout(l.residual, l.hidden, l.attnNormOut),
        )

      // FFN with FUSED Gate/Up/SwiGLU
      pipeline = pipeline
        .addProgram(F16CopyProgram.forward(copySizes))(
          _ => copySizes,
          l => F16CopyProgram.ProgramLayout(l.attnNormOut, l.residual),
        )
        .addProgram(F16RMSNormProgram.forward(ffnNormSizes))(
          _ => ffnNormSizes,
          l => F16RMSNormProgram.ProgramLayout(l.attnNormOut, l.ffnNorm, l.ffnNormOut),
        )
        .addProgram(F16FusedGateUpSwiGLUProgram.forward(fusedGateUpSizes))(
          _ => fusedGateUpSizes,
          l => F16FusedGateUpSwiGLUProgram.ProgramLayout(l.ffnGate, l.ffnUp, l.ffnNormOut, l.ffnHidden),
        )
        .addProgram(F16MatmulVecHybridProgram.forward(downSizes))(
          _ => downSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.ffnDown, l.ffnHidden, l.ffnOut),
        )
        .addProgram(F16ResidualAddProgram.forward(resSizes))(
          _ => resSizes,
          l => F16ResidualAddProgram.ProgramLayout(l.residual, l.ffnOut, l.hidden),
        )
    end for

    // Final norm and output projection
    val finalNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, 0, C)
    val logitsSizes = F16OutputVec4Program.Sizes(B * T, C, V)

    pipeline
      .addProgram(F16RMSNormProgram.forward(finalNormSizes))(
        _ => finalNormSizes,
        l => F16RMSNormProgram.ProgramLayout(l.hidden, l.outputNorm, l.attnNormOut),
      )
      .addProgram(F16OutputVec4Program.forward(logitsSizes))(
        _ => logitsSizes,
        l => F16OutputVec4Program.ProgramLayout(l.attnNormOut, l.outputWeight, l.logits),
      )
  end buildF16KVCachedPipelineFused

  // ============= F16 KV Cached Pipeline Class =============
  
  /** F16 KV-Cached Pipeline for fast incremental inference.
    * 
    * Like LlamaF32Pipeline.F32KVCachedPipeline but all F16:
    *   - Prefill: Process all prompt tokens at once, fill KV cache from 0 to T-1
    *   - Decode: Process 1 token at a time, append to KV cache at seqLen, attend to full cache
    * 
    * This achieves O(1) complexity per generated token (vs O(T) without cache).
    * 
    * Performance measurement:
    *   - Excludes buffer allocation and weight upload time
    *   - Measures GPU execution time (after read forces sync)
    *   - Reports separate prefill and generate tok/s
    */
  class F16KVCachedPipeline(
    weights: F16ModelWeights,
    val config: LlamaConfig,
    maxSeqLen: Int = F16KVCachedAttention.MAX_SEQ_LEN,
    B: Int = 1,
    useFused: Boolean = true,  // Use fused kernels for reduced dispatch count
  )(using runtime: CyfraRuntime) extends LlamaPipeline:
    require(maxSeqLen <= F16KVCachedAttention.MAX_SEQ_LEN, 
      s"maxSeqLen=$maxSeqLen exceeds F16KVCachedAttention.MAX_SEQ_LEN=${F16KVCachedAttention.MAX_SEQ_LEN}")
    
    private val C = config.hiddenSize
    private val V = config.vocabSize
    private val L = config.numHiddenLayers
    private val NH = config.numAttentionHeads
    private val NKV = config.numKeyValueHeads
    private val headSize = config.headSize
    private val FFN = config.intermediateSize
    private val kvSize = NKV * headSize
    
    Logger.info(s"Uploading F16 weights: ${L} layers, ${V}×${C} vocab, maxSeqLen=$maxSeqLen")
    
    private val tokenEmbedBuf = allocateF16Buffer(V * C)
    copyF16BytesToBuffer(weights.tokenEmbed, tokenEmbedBuf)
    tokenEmbedBuf.rewind()
    
    private val attnNormBuf = allocateF16Buffer(L * C)
    private val wqBuf = allocateF16Buffer(L * C * C)
    private val wkBuf = allocateF16Buffer(L * C * kvSize)
    private val wvBuf = allocateF16Buffer(L * C * kvSize)
    private val woBuf = allocateF16Buffer(L * C * C)
    private val ffnNormBuf = allocateF16Buffer(L * C)
    private val ffnGateBuf = allocateF16Buffer(L * FFN * C)
    private val ffnUpBuf = allocateF16Buffer(L * FFN * C)
    private val ffnDownBuf = allocateF16Buffer(L * C * FFN)
    
    for (layer, layerIdx) <- weights.layers.zipWithIndex do
      copyF16BytesToBuffer(layer.attnNorm, attnNormBuf, layerIdx * C * 2)
      copyF16BytesToBuffer(layer.wq, wqBuf, layerIdx * C * C * 2)
      copyF16BytesToBuffer(layer.wk, wkBuf, layerIdx * C * kvSize * 2)
      copyF16BytesToBuffer(layer.wv, wvBuf, layerIdx * C * kvSize * 2)
      copyF16BytesToBuffer(layer.wo, woBuf, layerIdx * C * C * 2)
      copyF16BytesToBuffer(layer.ffnNorm, ffnNormBuf, layerIdx * C * 2)
      copyF16BytesToBuffer(layer.ffnGate, ffnGateBuf, layerIdx * FFN * C * 2)
      copyF16BytesToBuffer(layer.ffnUp, ffnUpBuf, layerIdx * FFN * C * 2)
      copyF16BytesToBuffer(layer.ffnDown, ffnDownBuf, layerIdx * C * FFN * 2)
    
    attnNormBuf.rewind(); wqBuf.rewind(); wkBuf.rewind(); wvBuf.rewind(); woBuf.rewind()
    ffnNormBuf.rewind(); ffnGateBuf.rewind(); ffnUpBuf.rewind(); ffnDownBuf.rewind()
    
    private val outputNormBuf = allocateF16Buffer(C)
    copyF16BytesToBuffer(weights.outputNorm, outputNormBuf)
    outputNormBuf.rewind()
    
    private val outputWeightBuf = allocateF16Buffer(V * C)
    copyF16BytesToBuffer(weights.output, outputWeightBuf)
    outputWeightBuf.rewind()
    
    Logger.info("F16 weights uploaded to GPU")
    
    // Pre-allocate decode buffers (reused across generate() calls)
    private val decodeTokenBuf = allocateIntBuffer(B * 1)
    private val decodeLogitsBuf = allocateF32Buffer(B * 1 * V)
    private val decodeLogitsArr = new Array[Float](V)
    private val attnParamsBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())
    
    // Pipeline cache to avoid recompilation
    private val pipelineCache = scala.collection.mutable.Map[(Int, Int), GExecution[F16PipelineParams, F16KVCachePipelineLayout, F16KVCachePipelineLayout]]()
    
    private def getOrBuildPipeline(T: Int, seqLen: Int): GExecution[F16PipelineParams, F16KVCachePipelineLayout, F16KVCachePipelineLayout] =
      pipelineCache.getOrElseUpdate((T, seqLen), 
        if useFused then buildF16KVCachedPipelineFused(config, B, T, maxSeqLen)
        else buildF16KVCachedPipeline(config, B, T, maxSeqLen)
      )
    
    // Current sequence length (updated after each forward)
    private var currentSeqLen: Int = 0
    
    /** Current position in sequence (for RoPE and masking). */
    def seqLen: Int = currentSeqLen
    
    /** Last generation statistics (null if generate() not called yet). */
    private var _lastStats: GenerationStats = null
    def lastStats: Option[GenerationStats] = Option(_lastStats)
    
    /** Generate tokens with KV cache - OPTIMIZED version.
      * 
      * Optimizations:
      *   - Pre-allocated buffers (no allocation during generation loop)
      *   - Single runUnsafe to keep KV cache on GPU
      *   - Early stopping via callback
      *   - Performance timing excludes setup, measures only GPU execution
      * 
      * Architecture note: We use a single runUnsafe with F16GenerationLayout because:
      *   1. KV cache must persist on GPU across decode steps
      *   2. Weights should only be uploaded once
      *   3. GBufferRegion deallocates on runUnsafe completion
      * 
      * @param promptTokens Input prompt tokens
      * @param maxNewTokens Maximum tokens to generate
      * @param sampleFn Sampling function (logits => token)
      * @param onToken Callback for each generated token
      * @param stopTokens Set of tokens that stop generation
      * @param reportStats If true, prints performance stats after generation
      * @return Array of generated tokens (not including prompt)
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
      
      // Rewind all weight buffers
      tokenEmbedBuf.rewind()
      attnNormBuf.rewind()
      wqBuf.rewind()
      wkBuf.rewind()
      wvBuf.rewind()
      woBuf.rewind()
      ffnNormBuf.rewind()
      ffnGateBuf.rewind()
      ffnUpBuf.rewind()
      ffnDownBuf.rewind()
      outputNormBuf.rewind()
      outputWeightBuf.rewind()
      
      // Prepare prefill token buffer (this is prompt-size-dependent, allocated each call)
      val prefillTokensBuf = allocateIntBuffer(B * prefillT)
      prefillTokensBuf.asIntBuffer().put(promptTokens)
      prefillTokensBuf.rewind()
      val prefillLogitsBuf = allocateF32Buffer(B * prefillT * V)
      val prefillLogitsArr = new Array[Float](prefillT * V)
      
      // Build pipelines (cached)
      val prefillPipeline = getOrBuildPipeline(prefillT, prefillT)
      val decodePipeline = getOrBuildPipeline(1, maxSeqLen)
      val prefillParams = F16PipelineParams(config, B, prefillT, 0)
      
      // Create attention params buffers
      val prefillAttnBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())
      prefillAttnBuf.putInt(prefillT)  // seqLen
      prefillAttnBuf.putInt(0)         // startPos
      prefillAttnBuf.flip()
      
      val decodeAttnBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())
      decodeAttnBuf.putInt(1)
      decodeAttnBuf.putInt(0)
      decodeAttnBuf.flip()
      
      // Timing accumulators
      var prefillStartNs = 0L
      var prefillEndNs = 0L
      var decodeTimeNs = 0L
      var shouldStop = false
      
      // Build execution region - single runUnsafe keeps KV cache alive
      // Prefill phase
      var region = GBufferRegion
        .allocate[F16GenerationLayout]
        .map: layout =>
          // Start timing just before GPU execution
          prefillStartNs = System.nanoTime()
          prefillPipeline.execute(prefillParams, layout.toPrefillLayout)
          layout
        .map: layout =>
          // Read logits (forces GPU sync) - timing checkpoint
          layout.prefillLogits.read(prefillLogitsBuf)
          prefillEndNs = System.nanoTime()
          prefillLogitsBuf.rewind()
          copyFromF32Buffer(prefillLogitsBuf, prefillLogitsArr)
          
          // Sample first token (NOT timed - this is CPU sampling)
          val lastPosLogits = prefillLogitsArr.slice((prefillT - 1) * V, prefillT * V)
          val firstToken = sampleFn(lastPosLogits)
          generatedTokens += firstToken
          onToken(firstToken)
          currentSeqLen = prefillT
          
          // Check stop condition
          if stopTokens.contains(firstToken) then
            shouldStop = true
          else
            // Write first decode token
            decodeTokenBuf.clear()
            decodeTokenBuf.asIntBuffer().put(Array(firstToken))
            decodeTokenBuf.rewind()
            layout.decodeToken.write(decodeTokenBuf)
          
          layout
      
      // Decode loop - build dynamically with actual early stopping
      // We use a loop to add decode steps, checking shouldStop after each
      var step = 0
      while step < maxNewTokens - 1 do
        val stepIdx = step
        val seqLen = prefillT + stepIdx + 1
        val startPos = seqLen - 1
        val decodeParams = F16PipelineParams(config, B, 1, startPos)
        
        region = region
          .map: layout =>
            if shouldStop then
              layout
            else
              // Update attention params
              attnParamsBuf.clear()
              attnParamsBuf.putInt(seqLen)
              attnParamsBuf.putInt(startPos)
              attnParamsBuf.flip()
              layout.decodeAttnParams.asInstanceOf[io.computenode.cyfra.dsl.binding.GBinding[AttentionParams]].write(attnParamsBuf, 0)
              
              // Start decode timing
              val stepStartNs = System.nanoTime()
              decodePipeline.execute(decodeParams, layout.toDecodeLayout)
              
              // Read logits (forces GPU sync)
              layout.decodeLogits.read(decodeLogitsBuf)
              decodeTimeNs += (System.nanoTime() - stepStartNs)
              decodeLogitsBuf.rewind()
              copyFromF32Buffer(decodeLogitsBuf, decodeLogitsArr)
              
              // Sample next token (NOT timed)
              val nextToken = sampleFn(decodeLogitsArr)
              generatedTokens += nextToken
              onToken(nextToken)
              currentSeqLen += 1
              
              // Check stop condition
              if stopTokens.contains(nextToken) then
                shouldStop = true
              else
                // Write next token for next iteration
                decodeTokenBuf.clear()
                decodeTokenBuf.asIntBuffer().put(Array(nextToken))
                decodeTokenBuf.rewind()
                layout.decodeToken.write(decodeTokenBuf)
              
              layout
        
        step += 1
      end while
      
      // Execute everything - weights uploaded ONCE, KV cache stays on GPU
      region.runUnsafe(
        init = F16GenerationLayout(
          // Scalar weights (embedding, norms)
          tokenEmbed = GBuffer[Float16](tokenEmbedBuf),
          attnNorm = GBuffer[Float16](attnNormBuf),
          ffnNorm = GBuffer[Float16](ffnNormBuf),
          outputNorm = GBuffer[Float16](outputNormBuf),
          // Vec4 weights (matmul - same bytes, Vec4 SPIR-V type!)
          wq = GBuffer[Vec4[Float16]](wqBuf),
          wk = GBuffer[Vec4[Float16]](wkBuf),
          wv = GBuffer[Vec4[Float16]](wvBuf),
          wo = GBuffer[Vec4[Float16]](woBuf),
          ffnGate = GBuffer[Vec4[Float16]](ffnGateBuf),
          ffnUp = GBuffer[Vec4[Float16]](ffnUpBuf),
          ffnDown = GBuffer[Vec4[Float16]](ffnDownBuf),
          outputWeight = GBuffer[Vec4[Float16]](outputWeightBuf),
          // KV cache (GPU-only, persists across prefill/decode)
          kCache = GBuffer[Float16](L * maxSeqLen * kvSize),
          vCache = GBuffer[Float16](L * maxSeqLen * kvSize),
          // Prefill buffers (T = promptLen)
          prefillTokens = GBuffer[Int32](prefillTokensBuf),
          prefillHidden = GBuffer[Float16](B * prefillT * C),
          prefillResidual = GBuffer[Float16](B * prefillT * C),
          prefillAttnNormOut = GBuffer[Float16](B * prefillT * C),
          prefillQ = GBuffer[Float16](B * prefillT * C),
          prefillK = GBuffer[Float16](B * prefillT * kvSize),
          prefillV = GBuffer[Float16](B * prefillT * kvSize),
          prefillQRoped = GBuffer[Float16](B * prefillT * C),
          prefillKRoped = GBuffer[Float16](B * prefillT * kvSize),
          prefillAttnOut = GBuffer[Float16](B * prefillT * C),
          prefillFfnNormOut = GBuffer[Float16](B * prefillT * C),
          prefillGate = GBuffer[Float16](B * prefillT * FFN),
          prefillUp = GBuffer[Float16](B * prefillT * FFN),
          prefillFfnHidden = GBuffer[Float16](B * prefillT * FFN),
          prefillFfnOut = GBuffer[Float16](B * prefillT * C),
          prefillLogits = GBuffer[Float32](prefillLogitsBuf),
          prefillAttnParams = GUniform[AttentionParams](prefillAttnBuf),
          // Decode buffers (T = 1) - pre-allocated, reused
          decodeToken = GBuffer[Int32](decodeTokenBuf),
          decodeHidden = GBuffer[Float16](B * 1 * C),
          decodeResidual = GBuffer[Float16](B * 1 * C),
          decodeAttnNormOut = GBuffer[Float16](B * 1 * C),
          decodeQ = GBuffer[Float16](B * 1 * C),
          decodeK = GBuffer[Float16](B * 1 * kvSize),
          decodeV = GBuffer[Float16](B * 1 * kvSize),
          decodeQRoped = GBuffer[Float16](B * 1 * C),
          decodeKRoped = GBuffer[Float16](B * 1 * kvSize),
          decodeAttnOut = GBuffer[Float16](B * 1 * C),
          decodeFfnNormOut = GBuffer[Float16](B * 1 * C),
          decodeGate = GBuffer[Float16](B * 1 * FFN),
          decodeUp = GBuffer[Float16](B * 1 * FFN),
          decodeFfnHidden = GBuffer[Float16](B * 1 * FFN),
          decodeFfnOut = GBuffer[Float16](B * 1 * C),
          decodeLogits = GBuffer[Float32](decodeLogitsBuf),
          decodeAttnParams = GUniform[AttentionParams](decodeAttnBuf),
        ),
        onDone = _ => (),
      )
      
      val prefillTimeMs = (prefillEndNs - prefillStartNs) / 1_000_000.0
      val decodeTimeMs = decodeTimeNs / 1_000_000.0
      val totalTimeMs = prefillTimeMs + decodeTimeMs
      
      _lastStats = GenerationStats(
        promptTokens = prefillT,
        generatedTokens = generatedTokens.length,
        prefillTimeMs = prefillTimeMs,
        decodeTimeMs = decodeTimeMs,
        totalTimeMs = totalTimeMs,
      )
      
      if reportStats then
        Logger.info(_lastStats.toString)
      
      generatedTokens.toArray
    end generate
    
    /** Legacy API: Prefill (for backward compatibility).
      * 
      * NOTE: This creates a new GPU allocation each call - use `generate()` for efficient inference.
      */
    def prefill(tokens: Array[Int]): Array[Float] =
      require(tokens.length <= maxSeqLen, s"Prompt length ${tokens.length} exceeds maxSeqLen=$maxSeqLen")
      currentSeqLen = 0
      val logits = forwardWithKVCache(tokens, startPos = 0)
      currentSeqLen = tokens.length
      val lastPosLogits = new Array[Float](V)
      System.arraycopy(logits, (tokens.length - 1) * V, lastPosLogits, 0, V)
      lastPosLogits
    
    /** Legacy API: Decode single token (for backward compatibility).
      * 
      * NOTE: This creates a new GPU allocation each call - use `generate()` for efficient inference.
      */
    def decode(token: Int): Array[Float] =
      require(currentSeqLen < maxSeqLen, s"Sequence length reached maxSeqLen=$maxSeqLen")
      val logits = forwardWithKVCache(Array(token), startPos = currentSeqLen)
      currentSeqLen += 1
      logits
    
    // Legacy KV cache buffers for prefill/decode API
    private lazy val legacyKCacheBuf = allocateF16Buffer(L * maxSeqLen * kvSize)
    private lazy val legacyVCacheBuf = allocateF16Buffer(L * maxSeqLen * kvSize)
    
    // Forward pass with KV cache (legacy API - has GPU-CPU roundtrip)
    private def forwardWithKVCache(tokens: Array[Int], startPos: Int): Array[Float] =
      val T = tokens.length
      val seqLen = startPos + T  // Total sequence length after this forward
      
      // Rewind weight buffers
      tokenEmbedBuf.rewind()
      attnNormBuf.rewind()
      wqBuf.rewind()
      wkBuf.rewind()
      wvBuf.rewind()
      woBuf.rewind()
      ffnNormBuf.rewind()
      ffnGateBuf.rewind()
      ffnUpBuf.rewind()
      ffnDownBuf.rewind()
      outputNormBuf.rewind()
      outputWeightBuf.rewind()
      legacyKCacheBuf.rewind()
      legacyVCacheBuf.rewind()
      
      // Token buffer
      val tokensBuf = allocateIntBuffer(B * T)
      tokensBuf.asIntBuffer().put(tokens)
      tokensBuf.rewind()
      
      // Get cached pipeline for this T and seqLen
      val pipeline = getOrBuildPipeline(T, seqLen)
      
      val pParams = F16PipelineParams(config, B, T, startPos)
      
      val logits = new Array[Float](B * T * V)
      val logitsBuf = allocateF32Buffer(B * T * V)
      
      // Create AttentionParams ByteBuffer (seqLen + startPos)
      val attnStartPos = seqLen - T
      val attnParamsBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())
      attnParamsBuf.putInt(seqLen)
      attnParamsBuf.putInt(attnStartPos)
      attnParamsBuf.flip()
      
      val region = GBufferRegion
        .allocate[F16KVCachePipelineLayout]
        .map(layout => pipeline.execute(pParams, layout))
      
      region.runUnsafe(
        init = F16KVCachePipelineLayout(
          tokens = GBuffer[Int32](tokensBuf),
          // Scalar weights
          tokenEmbed = GBuffer[Float16](tokenEmbedBuf),
          attnNorm = GBuffer[Float16](attnNormBuf),
          ffnNorm = GBuffer[Float16](ffnNormBuf),
          outputNorm = GBuffer[Float16](outputNormBuf),
          // Vec4 weights
          wq = GBuffer[Vec4[Float16]](wqBuf),
          wk = GBuffer[Vec4[Float16]](wkBuf),
          wv = GBuffer[Vec4[Float16]](wvBuf),
          wo = GBuffer[Vec4[Float16]](woBuf),
          ffnGate = GBuffer[Vec4[Float16]](ffnGateBuf),
          ffnUp = GBuffer[Vec4[Float16]](ffnUpBuf),
          ffnDown = GBuffer[Vec4[Float16]](ffnDownBuf),
          outputWeight = GBuffer[Vec4[Float16]](outputWeightBuf),
          // KV cache
          kCache = GBuffer[Float16](legacyKCacheBuf),
          vCache = GBuffer[Float16](legacyVCacheBuf),
          // Activations
          hidden = GBuffer[Float16](B * T * C),
          residual = GBuffer[Float16](B * T * C),
          attnNormOut = GBuffer[Float16](B * T * C),
          q = GBuffer[Float16](B * T * C),
          k = GBuffer[Float16](B * T * kvSize),
          v = GBuffer[Float16](B * T * kvSize),
          qRoped = GBuffer[Float16](B * T * C),
          kRoped = GBuffer[Float16](B * T * kvSize),
          attnOut = GBuffer[Float16](B * T * C),
          ffnNormOut = GBuffer[Float16](B * T * C),
          gate = GBuffer[Float16](B * T * FFN),
          up = GBuffer[Float16](B * T * FFN),
          ffnHidden = GBuffer[Float16](B * T * FFN),
          ffnOut = GBuffer[Float16](B * T * C),
          logits = GBuffer[Float32](logitsBuf),
          attnParams = GUniform[AttentionParams](attnParamsBuf),
        ),
        onDone = layout =>
          // Read back KV cache (for next iteration) - THIS IS THE ROUNDTRIP
          layout.kCache.read(legacyKCacheBuf)
          layout.vCache.read(legacyVCacheBuf)
          layout.logits.read(logitsBuf)
          copyFromF32Buffer(logitsBuf, logits),
      )
      
      logits
    end forwardWithKVCache
  end F16KVCachedPipeline

end LlamaF16Pipeline
