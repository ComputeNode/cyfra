package io.computenode.cyfra.llama.pipeline

import io.computenode.cyfra.llama.model.LlamaConfig

import java.nio.{ByteBuffer, ByteOrder}

/** Common interface for Llama GPU pipelines.
  *
  * Defines the standard API for KV-cached inference pipelines:
  *   - generate: Efficient generation with prefill + decode in single GPU allocation
  *   - prefill: Process prompt tokens (legacy API)
  *   - decode: Generate one token (legacy API)
  *
  * Implementations:
  *   - LlamaF16Pipeline.F16KVCachedPipeline: F16 precision with Vec4 optimizations
  *   - LlamaF32Pipeline.F32KVCachedPipeline: F32/quantized precision (Q4_K/Q6_K)
  */
trait LlamaPipeline:

  /** Model configuration. */
  def config: LlamaConfig

  /** Current sequence length (position in KV cache). */
  def seqLen: Int

  /** Last generation statistics. */
  def lastStats: Option[GenerationStats]

  /** Generate tokens with KV cache.
    *
    * Optimized generation that keeps KV cache on GPU:
    *   - Prefill: Process all prompt tokens at once
    *   - Decode: Generate tokens one at a time, attending to full cache
    *
    * @param promptTokens Input prompt tokens
    * @param maxNewTokens Maximum tokens to generate
    * @param sampleFn Sampling function (logits => token)
    * @param onToken Callback for each generated token
    * @param stopTokens Set of tokens that stop generation
    * @param reportStats If true, logs performance stats after generation
    * @return Array of generated tokens (not including prompt)
    */
  def generate(
    promptTokens: Array[Int],
    maxNewTokens: Int,
    sampleFn: Array[Float] => Int,
    onToken: Int => Unit,
    stopTokens: Set[Int],
    reportStats: Boolean,
  ): Array[Int]

  /** Process prompt tokens and return logits for last position.
    *
    * @note Legacy API - creates new GPU allocation. Use generate() for efficient inference.
    */
  def prefill(tokens: Array[Int]): Array[Float]

  /** Generate next token logits.
    *
    * @note Legacy API - creates new GPU allocation. Use generate() for efficient inference.
    */
  def decode(token: Int): Array[Float]

/** Performance metrics from generation. */
case class GenerationStats(
  promptTokens: Int,
  generatedTokens: Int,
  prefillTimeMs: Double,
  decodeTimeMs: Double,
  totalTimeMs: Double,
):
  def prefillTokPerSec: Double = if prefillTimeMs > 0 then promptTokens * 1000.0 / prefillTimeMs else 0
  def decodeTokPerSec: Double = if decodeTimeMs > 0 then generatedTokens * 1000.0 / decodeTimeMs else 0
  def totalTokPerSec: Double = if totalTimeMs > 0 then (promptTokens + generatedTokens) * 1000.0 / totalTimeMs else 0

  override def toString: String =
    f"Gen: ${promptTokens}p+${generatedTokens}g, prefill=${prefillTokPerSec}%.0f tok/s, generate=${decodeTokPerSec}%.1f tok/s"

/** Buffer utilities for pipeline implementations. */
object PipelineUtils:

  def allocateF32Buffer(floatCount: Int): ByteBuffer =
    ByteBuffer.allocateDirect(floatCount * 4).order(ByteOrder.nativeOrder())

  def allocateF16Buffer(f16Count: Int): ByteBuffer =
    ByteBuffer.allocateDirect(f16Count * 2).order(ByteOrder.nativeOrder())

  def allocateIntBuffer(intCount: Int): ByteBuffer =
    ByteBuffer.allocateDirect(intCount * 4).order(ByteOrder.nativeOrder())

  def copyToF32Buffer(arr: Array[Float], buf: ByteBuffer): Unit =
    buf.clear(); buf.asFloatBuffer().put(arr); buf.rewind()

  def copyFromF32Buffer(buf: ByteBuffer, arr: Array[Float]): Unit =
    buf.rewind(); buf.asFloatBuffer().get(arr)

  def copyIntToBuffer(arr: Array[Int], buf: ByteBuffer): Unit =
    buf.clear(); buf.asIntBuffer().put(arr); buf.rewind()

  def copyF16BytesToBuffer(bytes: Array[Byte], buf: ByteBuffer, offset: Int = 0): Unit =
    buf.position(offset)
    buf.put(bytes)
