package io.computenode.cyfra.llama.inference

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llama.gguf.GGUFReader
import io.computenode.cyfra.llama.gguf.GGUFReader.{QuantType, TensorInfo}
import io.computenode.cyfra.llama.model.{LlamaConfig, LlamaModel}
import io.computenode.cyfra.llama.pipeline.{LlamaF32Pipeline, LlamaF16Pipeline}
import io.computenode.cyfra.llama.util.Logger
import io.computenode.cyfra.runtime.VkCyfraRuntime

/** Llama inference engine.
  * 
  * Loads weights from GGUF and runs the forward pass on GPU.
  * 
  * Supports two pipeline modes:
  *   - KVCachedPipeline: For quantized (Q4_K/Q6_K) models
  *   - F16KVCachedPipeline: For F16-native models (like Llama 3.2)
  * 
  * @param model The loaded Llama model
  * @param maxT Maximum sequence length for the pipeline
  * @param useQuantized If true, load quantized weights for KVCachedPipeline
  */
class LlamaInference(model: LlamaModel, maxT: Int = 1, useQuantized: Boolean = false)(using runtime: VkCyfraRuntime):
  val config: LlamaConfig = model.config
  
  private lazy val allWeightsAreF16: Boolean =
    val f32Tensors = model.gguf.tensors.filter(_.quantType == QuantType.F32)
    val f16Tensors = model.gguf.tensors.filter(_.quantType == QuantType.F16)
    Logger.debug(s"Model: ${f16Tensors.length} F16, ${f32Tensors.length} F32 tensors")
    val hasF16Weights = f16Tensors.exists(t => t.name.contains("weight"))
    val f32WeightMatrices = f32Tensors.filter(t => 
      t.name.contains("attn_q") || t.name.contains("attn_k") || t.name.contains("attn_v") || 
      t.name.contains("attn_output") || t.name.contains("ffn_gate") || t.name.contains("ffn_up") || 
      t.name.contains("ffn_down") || t.name.contains("token_embd") || t.name == "output.weight"
    )
    hasF16Weights && f32WeightMatrices.isEmpty
  
  /** Check if weights are a mix of Q4_K and Q6_K (common in TinyLlama). */
  private lazy val hasMixedQuantization: Boolean =
    val hasQ4K = model.gguf.tensors.exists(_.quantType == QuantType.Q4_K)
    val hasQ6K = model.gguf.tensors.exists(_.quantType == QuantType.Q6_K)
    hasQ4K && hasQ6K
  
  // Mixed quantization weights for KVCachedPipeline
  private lazy val mixedQuantWeights = if useQuantized && hasMixedQuantization then Some(loadMixedQuantWeights()) else None
  
  private lazy val f32KVCachedPipeline: LlamaF32Pipeline.F32KVCachedPipeline =
    require(mixedQuantWeights.isDefined, "Mixed quant weights not loaded. Set useQuantized=true.")
    new LlamaF32Pipeline.F32KVCachedPipeline(mixedQuantWeights.get, config, maxT)
  
  // F16-Native Pipeline for F16 models (KV-cached only)
  private lazy val f16Weights = if allWeightsAreF16 then Some(loadF16Weights()) else None
  
  // F16-Native KV Cached Pipeline with Vec4 optimizations (4x weight bandwidth!)
  private lazy val f16KVCachedPipeline: LlamaF16Pipeline.F16KVCachedPipeline =
    require(f16Weights.isDefined, "F16 KV pipeline requires F16 weights.")
    new LlamaF16Pipeline.F16KVCachedPipeline(f16Weights.get, config, maxT)
  
  /** Get the F32 KV-cached pipeline for quantized models.
    *
    * Uses Q4_K/Q6_K quantized weights with on-GPU dequantization.
    */
  def getF32KVCachedPipeline: LlamaF32Pipeline.F32KVCachedPipeline =
    require(useQuantized && hasMixedQuantization, "F32 KV cache requires quantized weights.")
    f32KVCachedPipeline
  
  /** Get the F16-native KV cached pipeline for efficient incremental inference.
    * 
    * This pipeline uses KV caching for O(1) per-token inference:
    *   - Prefill: Process all prompt tokens at once
    *   - Decode: Process 1 token at a time, attend to full KV cache
    * 
    * Uses Vec4-optimized matmuls for 4x weight memory bandwidth.
    * Requires all weights to be F16 quantized.
    * Requires dimensions (C, kvSize, FFN) to be divisible by 4.
    */
  def getF16KVCachedPipeline: LlamaF16Pipeline.F16KVCachedPipeline =
    require(f16Weights.isDefined, "F16 KV pipeline requires F16 weights. Check that model uses F16 quantization.")
    f16KVCachedPipeline

  private def loadMixedQuantWeights(): LlamaF32Pipeline.MixedQuantModelWeights =
    Logger.info(s"Loading mixed-quant weights (${config.numHiddenLayers} layers)...")
    val startTime = System.currentTimeMillis()
    
    val tokenEmbed = model.gguf.readTensorDequantized(model.getTensor(LlamaModel.TensorNames.tokenEmbed).get)
    val outputNorm = model.gguf.readTensorDequantized(model.getTensor(LlamaModel.TensorNames.outputNorm).get)
    val output = model.gguf.readTensorDequantized(model.getTensor(LlamaModel.TensorNames.output).get)
    
    def readQuantized(tensor: GGUFReader.TensorInfo): (Array[Int], LlamaF32Pipeline.QuantWeightType) =
      tensor.quantType match
        case QuantType.Q4_K => (model.gguf.readTensorQ4KAsUInt32(tensor), LlamaF32Pipeline.Q4K)
        case QuantType.Q6_K => (model.gguf.readTensorQ6KAsUInt32(tensor), LlamaF32Pipeline.Q6K)
        case other => throw new IllegalArgumentException(s"Unsupported quantization type: $other")
    
    val layers = (0 until config.numHiddenLayers).map: l =>
      val attnNorm = model.gguf.readTensorDequantized(model.getTensor(LlamaModel.TensorNames.attnNorm(l)).get)
      val ffnNorm = model.gguf.readTensorDequantized(model.getTensor(LlamaModel.TensorNames.ffnNorm(l)).get)
      val (wq, wqType) = readQuantized(model.getTensor(LlamaModel.TensorNames.attnQ(l)).get)
      val (wk, wkType) = readQuantized(model.getTensor(LlamaModel.TensorNames.attnK(l)).get)
      val (wv, wvType) = readQuantized(model.getTensor(LlamaModel.TensorNames.attnV(l)).get)
      val (wo, woType) = readQuantized(model.getTensor(LlamaModel.TensorNames.attnOutput(l)).get)
      val (ffnGate, ffnGateType) = readQuantized(model.getTensor(LlamaModel.TensorNames.ffnGate(l)).get)
      val (ffnUp, ffnUpType) = readQuantized(model.getTensor(LlamaModel.TensorNames.ffnUp(l)).get)
      val (ffnDown, ffnDownType) = readQuantized(model.getTensor(LlamaModel.TensorNames.ffnDown(l)).get)
      LlamaF32Pipeline.MixedQuantLayerWeights(
        attnNorm = attnNorm, wq = wq, wqType = wqType, wk = wk, wkType = wkType,
        wv = wv, wvType = wvType, wo = wo, woType = woType, ffnNorm = ffnNorm,
        ffnGate = ffnGate, ffnGateType = ffnGateType, ffnUp = ffnUp, ffnUpType = ffnUpType,
        ffnDown = ffnDown, ffnDownType = ffnDownType,
      )
    
    val elapsed = System.currentTimeMillis() - startTime
    val totalMB = layers.map(l => l.wq.length + l.wk.length + l.wv.length + l.wo.length +
      l.ffnGate.length + l.ffnUp.length + l.ffnDown.length).sum * 4 / 1024 / 1024
    Logger.info(s"Mixed-quant weights loaded: ${elapsed}ms, ${totalMB}MB")
    
    LlamaF32Pipeline.MixedQuantModelWeights(tokenEmbed, layers, outputNorm, output)

  /** Read tensor as F16 bytes, converting F32 to F16 if needed. */
  private def readAsF16Bytes(tensor: TensorInfo): Array[Byte] =
    if tensor.quantType == QuantType.F16 then
      model.gguf.readTensorF16Bytes(tensor)
    else if tensor.quantType == QuantType.F32 then
      val f32Array = model.gguf.readTensorDequantized(tensor)
      val f16Bytes = new Array[Byte](f32Array.length * 2)
      val buf = java.nio.ByteBuffer.wrap(f16Bytes).order(java.nio.ByteOrder.LITTLE_ENDIAN)
      for (f32Val, idx) <- f32Array.zipWithIndex do
        val f16Bits = floatToFloat16Bits(f32Val)
        buf.putShort(idx * 2, f16Bits.toShort)
      f16Bytes
    else
      throw new IllegalArgumentException(s"Cannot convert ${tensor.quantType} to F16")
  
  /** Convert F32 to F16 bits (IEEE 754 half precision). */
  private def floatToFloat16Bits(value: Float): Int =
    val bits = java.lang.Float.floatToRawIntBits(value)
    val sign = (bits >> 31) & 0x1
    val exp = (bits >> 23) & 0xFF
    val frac = bits & 0x7FFFFF
    
    if exp == 0xFF then
      return (sign << 15) | 0x7C00 | (if frac != 0 then 1 else 0)
    
    if exp == 0 && frac == 0 then
      return sign << 15
    
    val f16Exp = math.max(0, math.min(31, exp - 127 + 15))
    val f16Frac = frac >> 13
    (sign << 15) | (f16Exp << 10) | f16Frac
  
  private def loadF16Weights(): LlamaF16Pipeline.F16ModelWeights =
    Logger.info(s"Loading F16 weights (${config.numHiddenLayers} layers)...")
    val startTime = System.currentTimeMillis()
    
    val tokenEmbed = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.tokenEmbed).get)
    val outputNorm = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.outputNorm).get)
    val output = model.getTensor(LlamaModel.TensorNames.output) match
      case Some(tensor) => readAsF16Bytes(tensor)
      case None =>
        Logger.debug("Using tied embeddings (no output.weight)")
        tokenEmbed
    
    val layers = (0 until config.numHiddenLayers).map: l =>
      LlamaF16Pipeline.F16LayerWeights(
        attnNorm = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.attnNorm(l)).get),
        wq = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.attnQ(l)).get),
        wk = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.attnK(l)).get),
        wv = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.attnV(l)).get),
        wo = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.attnOutput(l)).get),
        ffnNorm = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.ffnNorm(l)).get),
        ffnGate = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.ffnGate(l)).get),
        ffnUp = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.ffnUp(l)).get),
        ffnDown = readAsF16Bytes(model.getTensor(LlamaModel.TensorNames.ffnDown(l)).get),
      )
    
    val elapsed = System.currentTimeMillis() - startTime
    val totalMB = (tokenEmbed.length + outputNorm.length + output.length +
      layers.map(l => l.attnNorm.length + l.wq.length + l.wk.length + l.wv.length + l.wo.length +
        l.ffnNorm.length + l.ffnGate.length + l.ffnUp.length + l.ffnDown.length).sum) / 1024 / 1024
    Logger.info(s"F16 weights loaded: ${elapsed}ms, ${totalMB}MB")
    
    LlamaF16Pipeline.F16ModelWeights(tokenEmbed, layers, outputNorm, output)

object LlamaInference:
  def apply(model: LlamaModel)(using runtime: VkCyfraRuntime): LlamaInference =
    new LlamaInference(model)
