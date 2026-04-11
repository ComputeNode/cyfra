package io.computenode.cyfra.llama.pipeline

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llama.model.LlamaConfig
import io.computenode.cyfra.llama.pipeline.LlamaF16Pipeline.*
import io.computenode.cyfra.llama.pipeline.PipelineUtils.*
import io.computenode.cyfra.llama.programs.*
import io.computenode.cyfra.llama.programs.f16.*
import io.computenode.cyfra.llama.util.Logger
import io.computenode.cyfra.utility.NVTX

import java.nio.{ByteBuffer, ByteOrder}

/** F16-Native Llama GPU Pipeline for fast incremental inference.
  *
  * All compute in half precision for maximum memory efficiency:
  *   - F16 weights loaded directly from GGUF (no conversion)
  *   - F16 compute throughout (matmul, attention, FFN)
  *   - Only final logits in F32 (for softmax stability)
  *   - 2x memory savings vs F32 pipeline
  *
  * KV-cached inference:
  *   - Prefill: Process all prompt tokens at once, fill KV cache
  *   - Decode: Process 1 token at a time, attend to full cache
  *   - O(1) complexity per generated token
  */
case class LlamaF16Pipeline(
  weights: F16ModelWeights,
  config: LlamaConfig,
  maxSeqLen: Int = DefaultMaxSeqLen,
  B: Int = 1,
)(using runtime: CyfraRuntime) extends LlamaPipeline:

  private val C = config.hiddenSize
  private val V = config.vocabSize
  private val L = config.numHiddenLayers
  private val NH = config.numAttentionHeads
  private val NKV = config.numKeyValueHeads
  private val headSize = config.headSize
  private val FFN = config.intermediateSize
  private val kvSize = NKV * headSize

  Logger.info(s"Uploading F16 weights: $L layers, ${V}×${C} vocab, maxSeqLen=$maxSeqLen")

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

  private val decodeTokenBuf = allocateIntBuffer(B * 1)
  private val decodeLogitsBuf = allocateF32Buffer(B * 1 * V)
  private val prefillLogitsArr = new Array[Float](V)
  private val attnParamsBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())

  // GPU sampling buffers
  private val sampleParamsBuf = ByteBuffer.allocateDirect(16).order(ByteOrder.nativeOrder()) // std140 aligned
  private val sampledTokenBuf = allocateIntBuffer(1)
  private val random = new scala.util.Random()

  // CPU sampling for prefill (can't use decode pipeline for variable-length prefill)
  private def cpuSample(logits: Array[Float], temperature: Float, topP: Float): Int =
    if temperature < 0.001f then
      var maxIdx = 0
      var maxVal = logits(0)
      var i = 1
      while i < logits.length do
        if logits(i) > maxVal then
          maxVal = logits(i)
          maxIdx = i
        i += 1
      maxIdx
    else
      val scaled = logits.map(_ / temperature)
      val maxLogit = scaled.max
      val expLogits = scaled.map(x => math.exp(x - maxLogit).toFloat)
      val sumExp = expLogits.sum
      val probs = expLogits.map(_ / sumExp)
      val indexed = probs.zipWithIndex.sortBy(-_._1)
      var cumSum = 0.0f
      var cutoffIdx = 0
      while cutoffIdx < indexed.length && cumSum < topP do
        cumSum += indexed(cutoffIdx)._1
        cutoffIdx += 1
      val topTokens = indexed.take(cutoffIdx)
      val topSum = topTokens.map(_._1).sum
      val threshold = random.nextFloat() * topSum
      var acc = 0.0f
      var result = topTokens.last._2
      for (prob, idx) <- topTokens do
        acc += prob
        if acc >= threshold && result == topTokens.last._2 then
          result = idx
      result

  private val pipelineCache = scala.collection.mutable.Map[(Int, Int, Boolean), GExecution[PipelineParams, PipelineLayout, PipelineLayout]]()

  private def getOrBuildPipeline(T: Int, seqLen: Int, withSampling: Boolean = false): GExecution[PipelineParams, PipelineLayout, PipelineLayout] =
    pipelineCache.getOrElseUpdate((T, seqLen, withSampling), buildPipeline(config, B, T, maxSeqLen, withSampling))

  // Decode pipeline with GPU sampling appended

  private var currentSeqLen: Int = 0

  def seqLen: Int = currentSeqLen

  private var _lastStats: GenerationStats = null
  def lastStats: Option[GenerationStats] = Option(_lastStats)

  def generate(
    promptTokens: Array[Int],
    maxNewTokens: Int,
    temperature: Float = 0.7f,
    topP: Float = 0.9f,
    onToken: Int => Unit = _ => (),
    stopTokens: Set[Int] = Set.empty,
    reportStats: Boolean = false,
  ): Array[Int] =
    require(
      promptTokens.length + maxNewTokens <= maxSeqLen,
      s"Total sequence ${promptTokens.length + maxNewTokens} exceeds maxSeqLen=$maxSeqLen",
    )

    currentSeqLen = 0
    val generatedTokens = scala.collection.mutable.ArrayBuffer[Int]()
    val prefillT = promptTokens.length

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

    val prefillTokensBuf = allocateIntBuffer(B * prefillT)
    prefillTokensBuf.asIntBuffer().put(promptTokens)
    prefillTokensBuf.rewind()
    val prefillLogitsBuf = allocateF32Buffer(B * prefillT * V)

    val prefillPipeline = getOrBuildPipeline(prefillT, prefillT, withSampling = false)
    val decodePipeline = getOrBuildPipeline(1, maxSeqLen, withSampling = true)
    val prefillParams = PipelineParams(config, B, prefillT, 0)

    val prefillAttnBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())
    prefillAttnBuf.putInt(prefillT)
    prefillAttnBuf.putInt(0)
    prefillAttnBuf.flip()

    val decodeAttnBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())
    decodeAttnBuf.putInt(prefillT + 1)
    decodeAttnBuf.putInt(prefillT)
    decodeAttnBuf.flip()

    var prefillStartNs = 0L
    var prefillEndNs = 0L
    var decodeTimeNs = 0L
    var shouldStop = false

    val afterPrefill = GBufferRegion
      .allocate[GenerationLayout]
      .map: layout =>
        NVTX.push(s"Prefill[$prefillT]")
        prefillStartNs = System.nanoTime()
        prefillPipeline.execute(prefillParams, layout.toPipelineLayout)
        layout
      .map: layout =>
        layout.prefillLogits.read(prefillLogitsBuf)
        prefillEndNs = System.nanoTime()
        NVTX.pop()
        prefillLogitsBuf.rewind()

        // Extract last position logits for sampling
        val prefillLogitsArr = new Array[Float](prefillT * V)
        copyFromF32Buffer(prefillLogitsBuf, prefillLogitsArr)
        val lastPosLogits = prefillLogitsArr.slice((prefillT - 1) * V, prefillT * V)

        val firstToken = cpuSample(lastPosLogits, temperature, topP)
        generatedTokens += firstToken
        onToken(firstToken)
        currentSeqLen = prefillT

        if stopTokens.contains(firstToken) then shouldStop = true
        else
          decodeTokenBuf.clear()
          decodeTokenBuf.asIntBuffer().put(Array(firstToken))
          decodeTokenBuf.rewind()
          layout.decodeToken.write(decodeTokenBuf)

        layout

    val afterDecode = (0 until maxNewTokens - 1).foldLeft(afterPrefill): (region, stepIdx) =>
      val seqLen = prefillT + stepIdx + 1
      val startPos = seqLen - 1
      val decodeParams = PipelineParams(config, B, 1, startPos)

      region.map: layout =>
        if shouldStop then layout
        else
          attnParamsBuf.clear()
          attnParamsBuf.putInt(seqLen)
          attnParamsBuf.putInt(startPos)
          attnParamsBuf.flip()
          layout.decodeAttnParams.asInstanceOf[io.computenode.cyfra.dsl.binding.GBinding[AttentionParams]].write(attnParamsBuf, 0)

          // Set sampling params
          sampleParamsBuf.clear()
          sampleParamsBuf.putFloat(temperature)
          sampleParamsBuf.putFloat(topP)
          sampleParamsBuf.putFloat(random.nextFloat())
          sampleParamsBuf.putFloat(0.0f)
          sampleParamsBuf.rewind()
          layout.sampleParams.asInstanceOf[io.computenode.cyfra.dsl.binding.GBinding[F16TopPSampleProgram.SampleParams]].write(sampleParamsBuf, 0)

          NVTX.push(s"Decode[$stepIdx]")
          val stepStartNs = System.nanoTime()
          NVTX.push(s"Execute[$stepIdx]")
          decodePipeline.execute(decodeParams, layout.toDecodeLayout)
          NVTX.pop()

          // Read sampled token
          layout.sampledToken.read(sampledTokenBuf)
          decodeTimeNs += (System.nanoTime() - stepStartNs)
          NVTX.pop()
          sampledTokenBuf.rewind()
          val nextToken = sampledTokenBuf.asIntBuffer().get(0)
          generatedTokens += nextToken
          onToken(nextToken)
          currentSeqLen += 1

          if stopTokens.contains(nextToken) then shouldStop = true
          else
            decodeTokenBuf.clear()
            decodeTokenBuf.asIntBuffer().put(Array(nextToken))
            decodeTokenBuf.rewind()
            layout.decodeToken.write(decodeTokenBuf)

          layout

    afterDecode.runUnsafe(
      init = GenerationLayout(
        tokenEmbed = GBuffer[Float16](tokenEmbedBuf),
        attnNorm = GBuffer[Float16](attnNormBuf),
        ffnNorm = GBuffer[Float16](ffnNormBuf),
        outputNorm = GBuffer[Float16](outputNormBuf),
        wq = GBuffer[Vec4[Float16]](wqBuf),
        wk = GBuffer[Vec4[Float16]](wkBuf),
        wv = GBuffer[Vec4[Float16]](wvBuf),
        wo = GBuffer[Vec4[Float16]](woBuf),
        ffnGate = GBuffer[Vec4[Float16]](ffnGateBuf),
        ffnUp = GBuffer[Vec4[Float16]](ffnUpBuf),
        ffnDown = GBuffer[Vec4[Float16]](ffnDownBuf),
        outputWeight = GBuffer[Vec4[Float16]](outputWeightBuf),
        kCache = GBuffer[Float16](L * maxSeqLen * kvSize),
        vCache = GBuffer[Float16](L * maxSeqLen * kvSize),
        prefillTokens = GBuffer[Int32](prefillTokensBuf),
        prefillHidden = GBuffer[Float16](B * prefillT * C),
        prefillResidual = GBuffer[Float16](B * prefillT * C),
        prefillAttnNormOut = GBuffer[Float16](B * prefillT * C),
        prefillQ = GBuffer[Float16](B * prefillT * C),
        prefillK = GBuffer[Float16](B * prefillT * kvSize),
        prefillV = GBuffer[Float16](B * prefillT * kvSize),
        prefillQRoped = GBuffer[Float16](B * prefillT * C),
        prefillKRoped = GBuffer[Float16](B * prefillT * kvSize),
        prefillAttnScores = GBuffer[Float32](B * prefillT * NH * maxSeqLen),
        prefillAttnOut = GBuffer[Float16](B * prefillT * C),
        prefillFfnNormOut = GBuffer[Float16](B * prefillT * C),
        prefillGate = GBuffer[Float16](B * prefillT * FFN),
        prefillUp = GBuffer[Float16](B * prefillT * FFN),
        prefillFfnHidden = GBuffer[Float16](B * prefillT * FFN),
        prefillFfnOut = GBuffer[Float16](B * prefillT * C),
        prefillLogits = GBuffer[Float32](prefillLogitsBuf),
        prefillAttnParams = GUniform[AttentionParams](prefillAttnBuf),
        decodeToken = GBuffer[Int32](decodeTokenBuf),
        decodeHidden = GBuffer[Float16](B * 1 * C),
        decodeResidual = GBuffer[Float16](B * 1 * C),
        decodeAttnNormOut = GBuffer[Float16](B * 1 * C),
        decodeQ = GBuffer[Float16](B * 1 * C),
        decodeK = GBuffer[Float16](B * 1 * kvSize),
        decodeV = GBuffer[Float16](B * 1 * kvSize),
        decodeQRoped = GBuffer[Float16](B * 1 * C),
        decodeKRoped = GBuffer[Float16](B * 1 * kvSize),
        decodeAttnScores = GBuffer[Float32](B * 1 * NH * maxSeqLen),
        decodeAttnOut = GBuffer[Float16](B * 1 * C),
        decodeFfnNormOut = GBuffer[Float16](B * 1 * C),
        decodeGate = GBuffer[Float16](B * 1 * FFN),
        decodeUp = GBuffer[Float16](B * 1 * FFN),
        decodeFfnHidden = GBuffer[Float16](B * 1 * FFN),
        decodeFfnOut = GBuffer[Float16](B * 1 * C),
        decodeLogits = GBuffer[Float32](decodeLogitsBuf),
        decodeAttnParams = GUniform[AttentionParams](decodeAttnBuf),
        sampleParams = GUniform[F16TopPSampleProgram.SampleParams](sampleParamsBuf),
        sampledToken = GBuffer[Int32](sampledTokenBuf),
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

    if reportStats then Logger.info(_lastStats.toString)

    generatedTokens.toArray
  end generate

end LlamaF16Pipeline

object LlamaF16Pipeline:

  val DefaultMaxSeqLen = 2048

  case class PipelineParams(
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

  case class F16LayerWeights(
    attnNorm: Array[Byte],
    wq: Array[Byte],
    wk: Array[Byte],
    wv: Array[Byte],
    wo: Array[Byte],
    ffnNorm: Array[Byte],
    ffnGate: Array[Byte],
    ffnUp: Array[Byte],
    ffnDown: Array[Byte],
  )
  
  case class F16ModelWeights(
    tokenEmbed: Array[Byte],
    layers: Seq[F16LayerWeights],
    outputNorm: Array[Byte],
    output: Array[Byte],
  )

  case class PipelineLayout(
    tokens: GBuffer[Int32],
    tokenEmbed: GBuffer[Float16],
    attnNorm: GBuffer[Float16],
    ffnNorm: GBuffer[Float16],
    outputNorm: GBuffer[Float16],
    wq: GBuffer[Vec4[Float16]],
    wk: GBuffer[Vec4[Float16]],
    wv: GBuffer[Vec4[Float16]],
    wo: GBuffer[Vec4[Float16]],
    ffnGate: GBuffer[Vec4[Float16]],
    ffnUp: GBuffer[Vec4[Float16]],
    ffnDown: GBuffer[Vec4[Float16]],
    outputWeight: GBuffer[Vec4[Float16]],
    kCache: GBuffer[Float16],
    vCache: GBuffer[Float16],
    hidden: GBuffer[Float16],
    residual: GBuffer[Float16],
    attnNormOut: GBuffer[Float16],
    q: GBuffer[Float16],
    k: GBuffer[Float16],
    v: GBuffer[Float16],
    qRoped: GBuffer[Float16],
    kRoped: GBuffer[Float16],
    attnScores: GBuffer[Float32],
    attnOut: GBuffer[Float16],
    ffnNormOut: GBuffer[Float16],
    gate: GBuffer[Float16],
    up: GBuffer[Float16],
    ffnHidden: GBuffer[Float16],
    ffnOut: GBuffer[Float16],
    logits: GBuffer[Float32],
    attnParams: GUniform[AttentionParams],
    sampleParams: GUniform[F16TopPSampleProgram.SampleParams],
    sampledToken: GBuffer[Int32],
  ) derives Layout

  case class GenerationLayout(
    tokenEmbed: GBuffer[Float16],
    attnNorm: GBuffer[Float16],
    ffnNorm: GBuffer[Float16],
    outputNorm: GBuffer[Float16],
    wq: GBuffer[Vec4[Float16]],
    wk: GBuffer[Vec4[Float16]],
    wv: GBuffer[Vec4[Float16]],
    wo: GBuffer[Vec4[Float16]],
    ffnGate: GBuffer[Vec4[Float16]],
    ffnUp: GBuffer[Vec4[Float16]],
    ffnDown: GBuffer[Vec4[Float16]],
    outputWeight: GBuffer[Vec4[Float16]],
    kCache: GBuffer[Float16],
    vCache: GBuffer[Float16],
    prefillTokens: GBuffer[Int32],
    prefillHidden: GBuffer[Float16],
    prefillResidual: GBuffer[Float16],
    prefillAttnNormOut: GBuffer[Float16],
    prefillQ: GBuffer[Float16],
    prefillK: GBuffer[Float16],
    prefillV: GBuffer[Float16],
    prefillQRoped: GBuffer[Float16],
    prefillKRoped: GBuffer[Float16],
    prefillAttnScores: GBuffer[Float32],
    prefillAttnOut: GBuffer[Float16],
    prefillFfnNormOut: GBuffer[Float16],
    prefillGate: GBuffer[Float16],
    prefillUp: GBuffer[Float16],
    prefillFfnHidden: GBuffer[Float16],
    prefillFfnOut: GBuffer[Float16],
    prefillLogits: GBuffer[Float32],
    prefillAttnParams: GUniform[AttentionParams],
    decodeToken: GBuffer[Int32],
    decodeHidden: GBuffer[Float16],
    decodeResidual: GBuffer[Float16],
    decodeAttnNormOut: GBuffer[Float16],
    decodeQ: GBuffer[Float16],
    decodeK: GBuffer[Float16],
    decodeV: GBuffer[Float16],
    decodeQRoped: GBuffer[Float16],
    decodeKRoped: GBuffer[Float16],
    decodeAttnScores: GBuffer[Float32],
    decodeAttnOut: GBuffer[Float16],
    decodeFfnNormOut: GBuffer[Float16],
    decodeGate: GBuffer[Float16],
    decodeUp: GBuffer[Float16],
    decodeFfnHidden: GBuffer[Float16],
    decodeFfnOut: GBuffer[Float16],
    decodeLogits: GBuffer[Float32],
    decodeAttnParams: GUniform[AttentionParams],
    // Sampling buffers
    sampleParams: GUniform[F16TopPSampleProgram.SampleParams],
    sampledToken: GBuffer[Int32],
  ) derives Layout:

    def toPipelineLayout: PipelineLayout = PipelineLayout(
      tokens = prefillTokens,
      tokenEmbed = tokenEmbed, attnNorm = attnNorm, ffnNorm = ffnNorm, outputNorm = outputNorm,
      wq = wq, wk = wk, wv = wv, wo = wo,
      ffnGate = ffnGate, ffnUp = ffnUp, ffnDown = ffnDown, outputWeight = outputWeight,
      kCache = kCache, vCache = vCache,
      hidden = prefillHidden, residual = prefillResidual, attnNormOut = prefillAttnNormOut,
      q = prefillQ, k = prefillK, v = prefillV, qRoped = prefillQRoped, kRoped = prefillKRoped,
      attnScores = prefillAttnScores, attnOut = prefillAttnOut, ffnNormOut = prefillFfnNormOut,
      gate = prefillGate, up = prefillUp, ffnHidden = prefillFfnHidden, ffnOut = prefillFfnOut,
      logits = prefillLogits, attnParams = prefillAttnParams, sampleParams = sampleParams, sampledToken = sampledToken,
    )
    
    def toDecodeLayout: PipelineLayout = PipelineLayout(
      tokens = decodeToken,
      tokenEmbed = tokenEmbed, attnNorm = attnNorm, ffnNorm = ffnNorm, outputNorm = outputNorm,
      wq = wq, wk = wk, wv = wv, wo = wo,
      ffnGate = ffnGate, ffnUp = ffnUp, ffnDown = ffnDown, outputWeight = outputWeight,
      kCache = kCache, vCache = vCache,
      hidden = decodeHidden, residual = decodeResidual, attnNormOut = decodeAttnNormOut,
      q = decodeQ, k = decodeK, v = decodeV, qRoped = decodeQRoped, kRoped = decodeKRoped,
      attnScores = decodeAttnScores, attnOut = decodeAttnOut, ffnNormOut = decodeFfnNormOut,
      gate = decodeGate, up = decodeUp, ffnHidden = decodeFfnHidden, ffnOut = decodeFfnOut,
      logits = decodeLogits, attnParams = decodeAttnParams, sampleParams = sampleParams, sampledToken = sampledToken,
    )

  def buildPipeline(
    config: LlamaConfig,
    B: Int,
    T: Int,
    maxSeqLen: Int,
    withSampling: Boolean = false,
  ): GExecution[PipelineParams, PipelineLayout, PipelineLayout] =
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
    val copySizeBytes = B * T * C * 2

    require(C % 4 == 0, s"hiddenSize ($C) must be divisible by 4")
    require(kvSize % 4 == 0, s"kvSize ($kvSize) must be divisible by 4")
    require(FFN % 4 == 0, s"intermediateSize ($FFN) must be divisible by 4")

    val embSizes = F16EmbeddingProgram.Sizes(B * T, C, V)
    val finalNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, 0, C)
    val logitsSizes = F16OutputVec4Program.Sizes(B * T, C, V)

    val afterEmbedding = GExecution[PipelineParams, PipelineLayout]()
      .addProgram(F16EmbeddingProgram.forward(embSizes))(
        _ => embSizes,
        l => F16EmbeddingProgram.ProgramLayout(l.tokens, l.tokenEmbed, l.hidden),
      )

    val afterLayers = (0 until L).foldLeft(afterEmbedding): (pipeline, layer) =>
      val normOffset = layer * C
      val wqOffsetVec4 = layer * C * (C / 4)
      val wkOffsetVec4 = layer * C * (kvSize / 4)
      val wvOffsetVec4 = layer * C * (kvSize / 4)
      val woOffsetVec4 = layer * C * (C / 4)
      val ffnGateOffsetVec4 = layer * FFN * (C / 4)
      val ffnUpOffsetVec4 = layer * FFN * (C / 4)
      val ffnDownOffsetVec4 = layer * C * (FFN / 4)
      val kvCacheLayerOffset = layer * maxSeqLen * kvSize

      val attnNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, normOffset, L * C)
      val qkvSizes = F16FusedQKVMatmulProgram.Sizes(
        batchSize = B * T, inFeatures = C, qOutFeatures = C, kvOutFeatures = kvSize,
        wqOffsetVec4 = wqOffsetVec4, wkOffsetVec4 = wkOffsetVec4, wvOffsetVec4 = wvOffsetVec4,
        totalWqVec4 = L * C * (C / 4), totalWkVec4 = L * C * (kvSize / 4), totalWvVec4 = L * C * (kvSize / 4),
      )
      val fusedRopeSizes = F16FusedRoPEProgram.Sizes(B, T, NH, NKV, headSize, theta)
      val fusedKVWriteSizes = F16FusedKVCacheWriteProgram.Sizes(
        B, T, NKV, headSize, maxSeqLen, layer, startPos, kvCacheLayerOffset, kvCacheLayerOffset, L,
      )
      val attnScoresSizes = F16AttentionScoresProgram.Sizes(B, T, NH, NKV, headSize, maxSeqLen, kvCacheLayerOffset, L)
      val attnSoftmaxSizes = F16AttentionSoftmaxProgram.Sizes(B, T, NH, maxSeqLen)
      val attnOutputSizes = F16AttentionOutputProgram.Sizes(B, T, NH, NKV, headSize, maxSeqLen, kvCacheLayerOffset, L)
      val woSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, C, woOffsetVec4, L * C * (C / 4))
      val resSizes = F16ResidualAddProgram.Sizes(B * T * C)
      val ffnNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, normOffset, L * C)
      val gateSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, FFN, ffnGateOffsetVec4, L * FFN * (C / 4))
      val upSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, FFN, ffnUpOffsetVec4, L * FFN * (C / 4))
      val swiGluSizes = F16SwiGLUProgram.Sizes(B * T * FFN)
      val downSizes = F16MatmulVecHybridProgram.Sizes(B * T, FFN, C, ffnDownOffsetVec4, L * C * (FFN / 4))

      pipeline
        .addBufferCopy(l => (l.hidden, l.residual), copySizeBytes)
        .addProgram(F16RMSNormProgram.forward(attnNormSizes))(
        _ => attnNormSizes,
        l => F16RMSNormProgram.ProgramLayout(l.hidden, l.attnNorm, l.attnNormOut),
      )
        .addProgram(F16FusedQKVMatmulProgram.forward(qkvSizes))(
        _ => qkvSizes,
        l => F16FusedQKVMatmulProgram.ProgramLayout(l.wq, l.wk, l.wv, l.attnNormOut, l.q, l.k, l.v),
      )
        .addProgram(F16FusedRoPEProgram.forward(fusedRopeSizes))(
        _ => fusedRopeSizes,
        l => F16FusedRoPEProgram.ProgramLayout(l.q, l.k, l.qRoped, l.kRoped, l.attnParams),
      )
        .addProgram(F16FusedKVCacheWriteProgram.forward(fusedKVWriteSizes))(
        _ => fusedKVWriteSizes,
        l => F16FusedKVCacheWriteProgram.ProgramLayout(l.kRoped, l.v, l.kCache, l.vCache, l.attnParams),
      )
        .addProgram(F16AttentionScoresProgram.forward(attnScoresSizes))(
          _ => attnScoresSizes,
          l => F16AttentionScoresProgram.ProgramLayout(l.qRoped, l.kCache, l.attnScores, l.attnParams),
        )
        .addProgram(F16AttentionSoftmaxProgram.forward(attnSoftmaxSizes))(
          _ => attnSoftmaxSizes,
          l => F16AttentionSoftmaxProgram.ProgramLayout(l.attnScores, l.attnParams),
        )
        .addProgram(F16AttentionOutputProgram.forward(attnOutputSizes))(
          _ => attnOutputSizes,
          l => F16AttentionOutputProgram.ProgramLayout(l.attnScores, l.vCache, l.attnOut, l.attnParams),
        )
        .addProgram(F16MatmulVecHybridProgram.forward(woSizes))(
          _ => woSizes,
          l => F16MatmulVecHybridProgram.ProgramLayout(l.wo, l.attnOut, l.hidden),
        )
        .addProgram(F16ResidualAddProgram.forward(resSizes))(
          _ => resSizes,
          l => F16ResidualAddProgram.ProgramLayout(l.residual, l.hidden, l.attnNormOut),
        )
        .addBufferCopy(l => (l.attnNormOut, l.residual), copySizeBytes)
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

    val afterOutput = afterLayers
      .addProgram(F16RMSNormProgram.forward(finalNormSizes))(
        _ => finalNormSizes,
        l => F16RMSNormProgram.ProgramLayout(l.hidden, l.outputNorm, l.attnNormOut),
      )
      .addProgram(F16OutputVec4Program.forward(logitsSizes))(
        _ => logitsSizes,
        l => F16OutputVec4Program.ProgramLayout(l.attnNormOut, l.outputWeight, l.logits),
      )

    if withSampling then
      val sampleSizes = F16TopPSampleProgram.Sizes(V)
      afterOutput.addProgram(F16TopPSampleProgram.forward(sampleSizes))(
        _ => sampleSizes,
        l => F16TopPSampleProgram.ProgramLayout(l.logits, l.sampleParams, l.sampledToken),
      )
    else
      afterOutput