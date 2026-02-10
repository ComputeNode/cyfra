package io.computenode.cyfra.llama.pipeline

import io.computenode.cyfra.llama.model.LlamaConfig

import java.nio.{ByteBuffer, ByteOrder}

/** Common interface for Llama GPU pipelines.
  *
  * Defines the standard API for KV-cached inference pipelines:
  *   - generate: Efficient generation with prefill + decode in single GPU allocation
  *
  * Implementations:
  *   - LlamaF16Pipeline: F16 precision with Vec4 optimizations
  */
trait LlamaPipeline:

  /** Model configuration. */
  def config: LlamaConfig

  /** Current sequence length (position in KV cache). */
  def seqLen: Int

  /** Last generation statistics. */
  def lastStats: Option[GenerationStats]

  /** Generate tokens with KV cache (GPU sampling).
    *
    * Optimized generation that keeps KV cache and sampling on GPU:
    *   - Prefill: Process all prompt tokens at once
    *   - Decode: Generate tokens one at a time, attending to full cache
    *   - Sample: GPU-accelerated top-p sampling with temperature
    *
    * @param promptTokens Input prompt tokens
    * @param maxNewTokens Maximum tokens to generate
    * @param temperature Sampling temperature (0 = greedy)
    * @param topP Top-p (nucleus) sampling threshold
    * @param onToken Callback for each generated token
    * @param stopTokens Set of tokens that stop generation
    * @param reportStats If true, logs performance stats after generation
    * @return Array of generated tokens (not including prompt)
    */
  def generate(
    promptTokens: Array[Int],
    maxNewTokens: Int,
    temperature: Float = 0.7f,
    topP: Float = 0.9f,
    onToken: Int => Unit = _ => (),
    stopTokens: Set[Int] = Set.empty,
    reportStats: Boolean = false,
  ): Array[Int]


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
