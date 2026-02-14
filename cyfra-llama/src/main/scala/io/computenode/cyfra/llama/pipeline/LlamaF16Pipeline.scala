package io.computenode.cyfra.llama.pipeline

import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GExecution, Provenance}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
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

  // Concatenate layer weights into flat buffers
  private def concatLayerWeights(extract: F16LayerWeights => Array[Byte]): Array[Byte] =
    weights.layers.flatMap(l => extract(l).toSeq).toArray

  private val tokenEmbedBuf = allocateF16Buffer(V * C)
  copyF16BytesToBuffer(weights.tokenEmbed, tokenEmbedBuf)
  tokenEmbedBuf.rewind()

  private val attnNormBuf = allocateF16Buffer(L * C)
  copyF16BytesToBuffer(concatLayerWeights(_.attnNorm), attnNormBuf)
  attnNormBuf.rewind()

  private val wqBuf = allocateF16Buffer(L * C * C)
  copyF16BytesToBuffer(concatLayerWeights(_.wq), wqBuf)
  wqBuf.rewind()

  private val wkBuf = allocateF16Buffer(L * C * kvSize)
  copyF16BytesToBuffer(concatLayerWeights(_.wk), wkBuf)
  wkBuf.rewind()

  private val wvBuf = allocateF16Buffer(L * C * kvSize)
  copyF16BytesToBuffer(concatLayerWeights(_.wv), wvBuf)
  wvBuf.rewind()

  private val woBuf = allocateF16Buffer(L * C * C)
  copyF16BytesToBuffer(concatLayerWeights(_.wo), woBuf)
  woBuf.rewind()

  private val ffnNormBuf = allocateF16Buffer(L * C)
  copyF16BytesToBuffer(concatLayerWeights(_.ffnNorm), ffnNormBuf)
  ffnNormBuf.rewind()

  private val ffnGateBuf = allocateF16Buffer(L * C * FFN)
  copyF16BytesToBuffer(concatLayerWeights(_.ffnGate), ffnGateBuf)
  ffnGateBuf.rewind()

  private val ffnUpBuf = allocateF16Buffer(L * C * FFN)
  copyF16BytesToBuffer(concatLayerWeights(_.ffnUp), ffnUpBuf)
  ffnUpBuf.rewind()

  private val ffnDownBuf = allocateF16Buffer(L * FFN * C)
  copyF16BytesToBuffer(concatLayerWeights(_.ffnDown), ffnDownBuf)
  ffnDownBuf.rewind()

  private val outputNormBuf = allocateF16Buffer(C)
  copyF16BytesToBuffer(weights.outputNorm, outputNormBuf)
  outputNormBuf.rewind()

  private val outputWeightBuf = allocateF16Buffer(C * V)
  copyF16BytesToBuffer(weights.outputWeight, outputWeightBuf)
  outputWeightBuf.rewind()

  private val decodeTokenBuf = allocateIntBuffer(B * 1)
  private val decodeLogitsBuf = allocateF32Buffer(B * 1 * V)
  private val sampledTokenBuf = allocateIntBuffer(B * 1)

  private val random = new scala.util.Random()
  private val sampleParamsBuf = ByteBuffer.allocateDirect(12).order(ByteOrder.nativeOrder()) // 3 floats
  private val attnParamsBuf = ByteBuffer.allocateDirect(8).order(ByteOrder.nativeOrder())

  private def cpuSample(logits: Array[Float], temperature: Float, topP: Float): Int =
    if temperature <= 0f then
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

  private val pipelineCache = scala.collection.mutable.Map[(Int, Int, Boolean), PipelineLayout => PipelineLayout]()

  private def getOrBuildPipeline(T: Int, seqLen: Int, withSampling: Boolean = false): PipelineLayout => PipelineLayout =
    pipelineCache.getOrElseUpdate((T, seqLen, withSampling), buildPipeline(config, B, T, maxSeqLen, withSampling))

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

    runtime.withAllocation { allocation =>
      given Allocation = allocation

      // Create layout with all buffers
      val layout = GenerationLayout(
        tokenEmbed = allocation.buffer[Float16](tokenEmbedBuf),
        attnNorm = allocation.buffer[Float16](attnNormBuf),
        ffnNorm = allocation.buffer[Float16](ffnNormBuf),
        outputNorm = allocation.buffer[Float16](outputNormBuf),
        wq = allocation.buffer[Vec4[Float16]](wqBuf),
        wk = allocation.buffer[Vec4[Float16]](wkBuf),
        wv = allocation.buffer[Vec4[Float16]](wvBuf),
        wo = allocation.buffer[Vec4[Float16]](woBuf),
        ffnGate = allocation.buffer[Vec4[Float16]](ffnGateBuf),
        ffnUp = allocation.buffer[Vec4[Float16]](ffnUpBuf),
        ffnDown = allocation.buffer[Vec4[Float16]](ffnDownBuf),
        outputWeight = allocation.buffer[Vec4[Float16]](outputWeightBuf),
        kCache = allocation.buffer[Float16](L * maxSeqLen * kvSize),
        vCache = allocation.buffer[Float16](L * maxSeqLen * kvSize),
        prefillTokens = allocation.buffer[Int32](prefillTokensBuf),
        prefillHidden = allocation.buffer[Float16](B * prefillT * C),
        prefillResidual = allocation.buffer[Float16](B * prefillT * C),
        prefillAttnNormOut = allocation.buffer[Float16](B * prefillT * C),
        prefillQ = allocation.buffer[Float16](B * prefillT * C),
        prefillK = allocation.buffer[Float16](B * prefillT * kvSize),
        prefillV = allocation.buffer[Float16](B * prefillT * kvSize),
        prefillQRoped = allocation.buffer[Float16](B * prefillT * C),
        prefillKRoped = allocation.buffer[Float16](B * prefillT * kvSize),
        prefillAttnScores = allocation.buffer[Float32](B * prefillT * NH * maxSeqLen),
        prefillAttnOut = allocation.buffer[Float16](B * prefillT * C),
        prefillFfnNormOut = allocation.buffer[Float16](B * prefillT * C),
        prefillGate = allocation.buffer[Float16](B * prefillT * FFN),
        prefillUp = allocation.buffer[Float16](B * prefillT * FFN),
        prefillFfnHidden = allocation.buffer[Float16](B * prefillT * FFN),
        prefillFfnOut = allocation.buffer[Float16](B * prefillT * C),
        prefillLogits = allocation.buffer[Float32](prefillLogitsBuf),
        prefillAttnParams = allocation.uniform[AttentionParams](prefillAttnBuf),
        decodeToken = allocation.buffer[Int32](decodeTokenBuf),
        decodeHidden = allocation.buffer[Float16](B * 1 * C),
        decodeResidual = allocation.buffer[Float16](B * 1 * C),
        decodeAttnNormOut = allocation.buffer[Float16](B * 1 * C),
        decodeQ = allocation.buffer[Float16](B * 1 * C),
        decodeK = allocation.buffer[Float16](B * 1 * kvSize),
        decodeV = allocation.buffer[Float16](B * 1 * kvSize),
        decodeQRoped = allocation.buffer[Float16](B * 1 * C),
        decodeKRoped = allocation.buffer[Float16](B * 1 * kvSize),
        decodeAttnScores = allocation.buffer[Float32](B * 1 * NH * maxSeqLen),
        decodeAttnOut = allocation.buffer[Float16](B * 1 * C),
        decodeFfnNormOut = allocation.buffer[Float16](B * 1 * C),
        decodeGate = allocation.buffer[Float16](B * 1 * FFN),
        decodeUp = allocation.buffer[Float16](B * 1 * FFN),
        decodeFfnHidden = allocation.buffer[Float16](B * 1 * FFN),
        decodeFfnOut = allocation.buffer[Float16](B * 1 * C),
        decodeLogits = allocation.buffer[Float32](decodeLogitsBuf),
        decodeAttnParams = allocation.uniform[AttentionParams](decodeAttnBuf),
        sampleParams = allocation.uniform[F16TopPSampleProgram.SampleParams](sampleParamsBuf),
        sampledToken = allocation.buffer[Int32](sampledTokenBuf),
      )

      // === Prefill phase ===
      NVTX.push(s"Prefill[$prefillT]")
      prefillStartNs = System.nanoTime()

      // Run prefill pipeline (builds DAG)
      val afterPrefill = prefillPipeline(layout.toPipelineLayout)

      // Materialize to execute the prefill
      val materializedPrefill = allocation.materialize(afterPrefill)

      // Read prefill logits
      layout.prefillLogits.readTo(prefillLogitsBuf)
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
        layout.decodeToken.writeFrom(decodeTokenBuf)

      // === Decode phase ===
      var stepIdx = 0
      while stepIdx < maxNewTokens - 1 && !shouldStop do
        val seqLen = prefillT + stepIdx + 1
        val startPos = seqLen - 1

        attnParamsBuf.clear()
        attnParamsBuf.putInt(seqLen)
        attnParamsBuf.putInt(startPos)
        attnParamsBuf.flip()
        layout.decodeAttnParams.writeFrom(attnParamsBuf)

        // Set sampling params (3 floats: temperature, topP, randomValue)
        sampleParamsBuf.clear()
        sampleParamsBuf.putFloat(temperature)
        sampleParamsBuf.putFloat(topP)
        sampleParamsBuf.putFloat(random.nextFloat())
        sampleParamsBuf.rewind()
        layout.sampleParams.writeFrom(sampleParamsBuf)

        NVTX.push(s"Decode[$stepIdx]")
        val stepStartNs = System.nanoTime()
        NVTX.push(s"Execute[$stepIdx]")

        // Try fast path first (skips provenance building)
        val decodeLayout = layout.toDecodeLayout
        allocation match
          case vk: io.computenode.cyfra.runtime.VkAllocation =>
            if !vk.submitCached(decodeLayout) then
              // Cache miss - need full pipeline run + materialize
              val afterDecode = decodePipeline(decodeLayout)
              allocation.materialize(afterDecode)
          case _ =>
            // Fallback for non-VkAllocation
            val afterDecode = decodePipeline(decodeLayout)
            allocation.materialize(afterDecode)

        NVTX.pop()

        // Read sampled token from GPU (only 4 bytes!)
        layout.sampledToken.readTo(sampledTokenBuf)
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
          layout.decodeToken.writeFrom(decodeTokenBuf)

        stepIdx += 1
      end while
    }

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
    outputWeight: Array[Byte],
  )

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
    def kvSize: Int = NKV * headSize
    def seqLen: Int = startPos + T
    def FFN: Int = config.intermediateSize
    def L: Int = config.numHiddenLayers
    def V: Int = config.vocabSize

  // Define layouts
  case class PipelineLayout(
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
    tokens: GBuffer[Int32],
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
    // Weights
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
    // KV Cache
    kCache: GBuffer[Float16],
    vCache: GBuffer[Float16],
    // Prefill buffers
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
    // Decode buffers
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
    sampleParams: GUniform[F16TopPSampleProgram.SampleParams],
    sampledToken: GBuffer[Int32],
  ) derives Layout:
    def toPipelineLayout: PipelineLayout = PipelineLayout(
      tokenEmbed, attnNorm, ffnNorm, outputNorm,
      wq, wk, wv, wo, ffnGate, ffnUp, ffnDown, outputWeight,
      kCache, vCache,
      prefillTokens, prefillHidden, prefillResidual, prefillAttnNormOut,
      prefillQ, prefillK, prefillV, prefillQRoped, prefillKRoped,
      prefillAttnScores, prefillAttnOut, prefillFfnNormOut,
      prefillGate, prefillUp, prefillFfnHidden, prefillFfnOut, prefillLogits,
      prefillAttnParams, sampleParams, sampledToken,
    )

    def toDecodeLayout: PipelineLayout = PipelineLayout(
      tokenEmbed, attnNorm, ffnNorm, outputNorm,
      wq, wk, wv, wo, ffnGate, ffnUp, ffnDown, outputWeight,
      kCache, vCache,
      decodeToken, decodeHidden, decodeResidual, decodeAttnNormOut,
      decodeQ, decodeK, decodeV, decodeQRoped, decodeKRoped,
      decodeAttnScores, decodeAttnOut, decodeFfnNormOut,
      decodeGate, decodeUp, decodeFfnHidden, decodeFfnOut, decodeLogits,
      decodeAttnParams, sampleParams, sampledToken,
    )

  /** Run the F16 pipeline by chaining program dispatches.
    * Each program dispatch builds the provenance DAG.
    * Returns the layout with all buffers having proper provenance.
    */
  def runPipeline(
    config: LlamaConfig,
    B: Int,
    T: Int,
    maxSeqLen: Int,
    layout: PipelineLayout,
    withSampling: Boolean = false,
  ): PipelineLayout =
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

    // Embedding: tokens -> hidden
    val embSizes = F16EmbeddingProgram.Sizes(B * T, C, V)
    val embLayout = F16EmbeddingProgram.ProgramLayout(layout.tokens, layout.tokenEmbed, layout.hidden)
    val afterEmb = F16EmbeddingProgram.forward(embSizes).dispatch(embSizes, embLayout)

    // Track the current state of buffers
    var hidden = afterEmb.output
    var residual = layout.residual
    var attnNormOut = layout.attnNormOut
    var q = layout.q
    var k = layout.k
    var v = layout.v
    var qRoped = layout.qRoped
    var kRoped = layout.kRoped
    var attnOut = layout.attnOut
    var ffnNormOut = layout.ffnNormOut
    var gate = layout.gate
    var up = layout.up
    var ffnHidden = layout.ffnHidden
    var ffnOut = layout.ffnOut
    var kCache = layout.kCache
    var vCache = layout.vCache

    // Process each layer
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
      val copySizeElements = B * T * C

      // Save residual (copy hidden -> residual) - DMA copy
      residual = residual.withProvenance(Provenance.Copied(hidden, 0, 0, copySizeElements))

      // Attention norm
      val attnNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, normOffset, L * C)
      val normLayout = F16RMSNormProgram.ProgramLayout(hidden, layout.attnNorm, attnNormOut)
      val afterNorm = F16RMSNormProgram.forward(attnNormSizes).dispatch(attnNormSizes, normLayout)
      attnNormOut = afterNorm.output

      // Q, K, V projections
      val qSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, C, wqOffsetVec4, L * C * (C / 4))
      val kSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, kvSize, wkOffsetVec4, L * C * (kvSize / 4))
      val vSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, kvSize, wvOffsetVec4, L * C * (kvSize / 4))

      val qLayout = F16MatmulVecHybridProgram.ProgramLayout(layout.wq, attnNormOut, q)
      val afterQ = F16MatmulVecHybridProgram.forward(qSizes).dispatch(qSizes, qLayout)
      q = afterQ.output

      val kLayout = F16MatmulVecHybridProgram.ProgramLayout(layout.wk, attnNormOut, k)
      val afterK = F16MatmulVecHybridProgram.forward(kSizes).dispatch(kSizes, kLayout)
      k = afterK.output

      val vLayout = F16MatmulVecHybridProgram.ProgramLayout(layout.wv, attnNormOut, v)
      val afterV = F16MatmulVecHybridProgram.forward(vSizes).dispatch(vSizes, vLayout)
      v = afterV.output

      // Fused RoPE for both Q and K
      val ropeSizes = F16FusedRoPEProgram.Sizes(B, T, NH, NKV, headSize, theta)
      val ropeLayout = F16FusedRoPEProgram.ProgramLayout(q, k, qRoped, kRoped, layout.attnParams)
      val afterRope = F16FusedRoPEProgram.forward(ropeSizes).dispatch(ropeSizes, ropeLayout)
      qRoped = afterRope.qOut
      kRoped = afterRope.kOut

      // Fused KV Cache Write - write K and V to cache
      val kvWriteSizes = F16FusedKVCacheWriteProgram.Sizes(B, T, NKV, headSize, maxSeqLen, layer, startPos, kvCacheLayerOffset, kvCacheLayerOffset, L)
      val kvWriteLayout = F16FusedKVCacheWriteProgram.ProgramLayout(kRoped, v, kCache, vCache, layout.attnParams)
      val afterKVWrite = F16FusedKVCacheWriteProgram.forward(kvWriteSizes).dispatch(kvWriteSizes, kvWriteLayout)
      kCache = afterKVWrite.kCache
      vCache = afterKVWrite.vCache

      // Attention scores: Q · K^T
      val scoreSizes = F16AttentionScoresProgram.Sizes(B, T, NH, NKV, headSize, maxSeqLen, kvCacheLayerOffset, L)
      val scoreLayout = F16AttentionScoresProgram.ProgramLayout(qRoped, kCache, layout.attnScores, layout.attnParams)
      val afterScores = F16AttentionScoresProgram.forward(scoreSizes).dispatch(scoreSizes, scoreLayout)
      var attnScores = afterScores.scores

      // Attention softmax (in-place)
      val softmaxSizes = F16AttentionSoftmaxProgram.Sizes(B, T, NH, maxSeqLen)
      val softmaxLayout = F16AttentionSoftmaxProgram.ProgramLayout(attnScores, layout.attnParams)
      val afterSoftmax = F16AttentionSoftmaxProgram.forward(softmaxSizes).dispatch(softmaxSizes, softmaxLayout)
      attnScores = afterSoftmax.scores

      // Attention output: attn_weights · V
      val outputSizes = F16AttentionOutputProgram.Sizes(B, T, NH, NKV, headSize, maxSeqLen, kvCacheLayerOffset, L)
      val outputLayout = F16AttentionOutputProgram.ProgramLayout(attnScores, vCache, attnOut, layout.attnParams)
      val afterAttnOut = F16AttentionOutputProgram.forward(outputSizes).dispatch(outputSizes, outputLayout)
      attnOut = afterAttnOut.output

      // Output projection
      val woSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, C, woOffsetVec4, L * C * (C / 4))
      val woLayout = F16MatmulVecHybridProgram.ProgramLayout(layout.wo, attnOut, hidden)
      val afterWo = F16MatmulVecHybridProgram.forward(woSizes).dispatch(woSizes, woLayout)
      hidden = afterWo.output

      // Residual add
      val resSizes = F16ResidualAddProgram.Sizes(B * T * C)
      val resLayout = F16ResidualAddProgram.ProgramLayout(residual, hidden, attnNormOut)
      val afterRes = F16ResidualAddProgram.forward(resSizes).dispatch(resSizes, resLayout)
      attnNormOut = afterRes.output

      // FFN - copy attnNormOut -> residual before FFN
      residual = residual.withProvenance(Provenance.Copied(attnNormOut, 0, 0, copySizeElements))

      val ffnNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, ffnNormOffset, L * C)
      val ffnNormLayout = F16RMSNormProgram.ProgramLayout(attnNormOut, layout.ffnNorm, ffnNormOut)
      val afterFfnNorm = F16RMSNormProgram.forward(ffnNormSizes).dispatch(ffnNormSizes, ffnNormLayout)
      ffnNormOut = afterFfnNorm.output

      // Gate and Up projections
      val gateSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, FFN, ffnGateOffsetVec4, L * FFN * (C / 4))
      val gateLayout = F16MatmulVecHybridProgram.ProgramLayout(layout.ffnGate, ffnNormOut, gate)
      val afterGate = F16MatmulVecHybridProgram.forward(gateSizes).dispatch(gateSizes, gateLayout)
      gate = afterGate.output

      val upSizes = F16MatmulVecHybridProgram.Sizes(B * T, C, FFN, ffnUpOffsetVec4, L * FFN * (C / 4))
      val upLayout = F16MatmulVecHybridProgram.ProgramLayout(layout.ffnUp, ffnNormOut, up)
      val afterUp = F16MatmulVecHybridProgram.forward(upSizes).dispatch(upSizes, upLayout)
      up = afterUp.output

      // SwiGLU
      val swiGluSizes = F16SwiGLUProgram.Sizes(B * T * FFN)
      val swiGluLayout = F16SwiGLUProgram.ProgramLayout(gate, up, ffnHidden)
      val afterSwiglu = F16SwiGLUProgram.forward(swiGluSizes).dispatch(swiGluSizes, swiGluLayout)
      ffnHidden = afterSwiglu.output

      // Down projection
      val downSizes = F16MatmulVecHybridProgram.Sizes(B * T, FFN, C, ffnDownOffsetVec4, L * C * (FFN / 4))
      val downLayout = F16MatmulVecHybridProgram.ProgramLayout(layout.ffnDown, ffnHidden, ffnOut)
      val afterDown = F16MatmulVecHybridProgram.forward(downSizes).dispatch(downSizes, downLayout)
      ffnOut = afterDown.output

      // FFN residual add
      val ffnResLayout = F16ResidualAddProgram.ProgramLayout(residual, ffnOut, hidden)
      val afterFfnRes = F16ResidualAddProgram.forward(resSizes).dispatch(resSizes, ffnResLayout)
      hidden = afterFfnRes.output
    end for

    // Final norm and output projection
    val finalNormSizes = F16RMSNormProgram.Sizes(B * T, C, eps, 0, C)
    val finalNormLayout = F16RMSNormProgram.ProgramLayout(hidden, layout.outputNorm, attnNormOut)
    val afterFinalNorm = F16RMSNormProgram.forward(finalNormSizes).dispatch(finalNormSizes, finalNormLayout)
    attnNormOut = afterFinalNorm.output

    val logitsSizes = F16OutputVec4Program.Sizes(B * T, C, V)
    val logitsLayout = F16OutputVec4Program.ProgramLayout(attnNormOut, layout.outputWeight, layout.logits)
    val afterLogits = F16OutputVec4Program.forward(logitsSizes).dispatch(logitsSizes, logitsLayout)

    // Optionally add GPU sampling
    val finalSampledToken = if withSampling then
      val sampleSizes = F16TopPSampleProgram.Sizes(V)
      val sampleLayout = F16TopPSampleProgram.ProgramLayout(
        logits = afterLogits.output,
        params = layout.sampleParams,
        result = layout.sampledToken,
      )
      F16TopPSampleProgram.forward(sampleSizes).dispatch(sampleSizes, sampleLayout).result
    else
      layout.sampledToken

    // Return updated layout with all provenance tracked
    layout.copy(
      hidden = hidden,
      residual = residual,
      attnNormOut = attnNormOut,
      q = q,
      k = k,
      v = v,
      qRoped = qRoped,
      kRoped = kRoped,
      attnOut = attnOut,
      ffnNormOut = ffnNormOut,
      gate = gate,
      up = up,
      ffnHidden = ffnHidden,
      ffnOut = ffnOut,
      kCache = kCache,
      vCache = vCache,
      logits = afterLogits.output,
      sampledToken = finalSampledToken,
    )

  /** Build the pipeline for the given configuration.
    * Returns a GExecution that chains all F16 programs.
    */
  def buildPipeline(
    config: LlamaConfig,
    B: Int,
    T: Int,
    maxSeqLen: Int,
    withSampling: Boolean = false,
  ): PipelineLayout => PipelineLayout =
    layout => runPipeline(config, B, T, maxSeqLen, layout, withSampling)

end LlamaF16Pipeline
