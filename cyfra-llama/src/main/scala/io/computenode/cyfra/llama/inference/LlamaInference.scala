package io.computenode.cyfra.llama.inference

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llama.gguf.GGUFReader
import io.computenode.cyfra.llama.gguf.GGUFReader.{QuantType, TensorInfo}
import io.computenode.cyfra.llama.model.{LlamaConfig, LlamaModel}
import io.computenode.cyfra.llama.pipeline.LlamaF16Pipeline
import io.computenode.cyfra.llama.util.Logger
import io.computenode.cyfra.runtime.VkCyfraRuntime

/** Llama inference engine.
  *
  * Loads weights from GGUF and runs the forward pass on GPU.
  *
  * Supports two pipeline modes:
  *   - LlamaF32Pipeline: For quantized (Q4_K/Q6_K) models
  *   - LlamaF16Pipeline: For F16-native models (like Llama 3.2)
  *
  * @param model The loaded Llama model
  * @param maxT Maximum sequence length for the pipeline
  */
class LlamaInference(model: LlamaModel, maxT: Int = 1)(using runtime: VkCyfraRuntime):
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

  
  // F16-Native Pipeline for F16 models (KV-cached only)
  private lazy val f16Weights = if allWeightsAreF16 then Some(loadF16Weights()) else None
  
  // F16-Native KV Cached Pipeline with Vec4 optimizations (4x weight bandwidth!)
  private lazy val f16Pipeline: LlamaF16Pipeline =
    require(f16Weights.isDefined, "F16 KV pipeline requires F16 weights.")
    LlamaF16Pipeline(f16Weights.get, config, maxT)
  
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
  def getF16Pipeline: LlamaF16Pipeline =
    require(f16Weights.isDefined, "F16 pipeline requires F16 weights. Check that model uses F16 quantization.")
    f16Pipeline

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
