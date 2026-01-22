package io.computenode.cyfra.llama.model

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct

/** Llama model configuration.
  * 
  * Based on the Llama 2 / Llama 3 architecture with:
  *   - RMSNorm instead of LayerNorm
  *   - SiLU activation in MLP
  *   - Rotary Position Embeddings (RoPE)
  *   - Grouped Query Attention (GQA)
  *   - SwiGLU MLP structure
  */
case class LlamaConfig(
  hiddenSize: Int,             // Model dimension (d_model)
  intermediateSize: Int,       // MLP hidden dimension (usually ~2.7x hidden)
  numAttentionHeads: Int,      // Query heads
  numKeyValueHeads: Int,       // Key/Value heads (for GQA)
  numHiddenLayers: Int,        // Number of transformer blocks
  vocabSize: Int,              // Vocabulary size
  maxPositionEmbeddings: Int,  // Max context length
  rmsNormEps: Float = 1e-6f,   // RMSNorm epsilon
  ropeTheta: Float = 10000.0f, // RoPE base frequency
  bos_token_id: Int = 1,       // Beginning of sequence token
  eos_token_id: Int = 2,       // End of sequence token
):
  def headSize: Int = hiddenSize / numAttentionHeads
  def kvHeadSize: Int = hiddenSize / numKeyValueHeads
  def gqaRatio: Int = numAttentionHeads / numKeyValueHeads  // GQA ratio (1 = MHA, >1 = GQA)
  
  /** Total parameter count estimate (weights only, no embeddings counted separately) */
  def numParameters: Long =
    // Embeddings
    val embedParams = vocabSize.toLong * hiddenSize
    // Per-layer params
    val qkvParams = hiddenSize * (hiddenSize + 2 * (hiddenSize * numKeyValueHeads / numAttentionHeads))
    val outputParams = hiddenSize * hiddenSize
    val mlpParams = 3 * hiddenSize * intermediateSize  // gate, up, down projections
    val normParams = 2 * hiddenSize  // 2 RMSNorms per layer
    val perLayerParams = qkvParams + outputParams + mlpParams + normParams
    // Total
    embedParams + numHiddenLayers * perLayerParams + hiddenSize + embedParams  // final norm + output proj

object LlamaConfig:
  /** TinyLlama 1.1B configuration */
  val TinyLlama_1B: LlamaConfig = LlamaConfig(
    hiddenSize = 2048,
    intermediateSize = 5632,
    numAttentionHeads = 32,
    numKeyValueHeads = 4,
    numHiddenLayers = 22,
    vocabSize = 32000,
    maxPositionEmbeddings = 2048,
  )

  /** Llama 2 7B configuration */
  val Llama2_7B: LlamaConfig = LlamaConfig(
    hiddenSize = 4096,
    intermediateSize = 11008,
    numAttentionHeads = 32,
    numKeyValueHeads = 32,  // MHA (not GQA)
    numHiddenLayers = 32,
    vocabSize = 32000,
    maxPositionEmbeddings = 4096,
  )

  /** Llama 2 13B configuration */
  val Llama2_13B: LlamaConfig = LlamaConfig(
    hiddenSize = 5120,
    intermediateSize = 13824,
    numAttentionHeads = 40,
    numKeyValueHeads = 40,
    numHiddenLayers = 40,
    vocabSize = 32000,
    maxPositionEmbeddings = 4096,
  )

  /** Llama 3 8B configuration */
  val Llama3_8B: LlamaConfig = LlamaConfig(
    hiddenSize = 4096,
    intermediateSize = 14336,
    numAttentionHeads = 32,
    numKeyValueHeads = 8,  // GQA with ratio 4
    numHiddenLayers = 32,
    vocabSize = 128256,
    maxPositionEmbeddings = 8192,
    ropeTheta = 500000.0f,
  )

/** GPU-side parameters for Llama operations */
case class LlamaParams(
  B: Int32,       // Batch size
  T: Int32,       // Sequence length (current position for generation)
  C: Int32,       // Hidden size (channels)
  NH: Int32,      // Number of attention heads
  NKV: Int32,     // Number of key-value heads
  HS: Int32,      // Head size
  eps: Float32,   // RMSNorm epsilon
) extends GStruct[LlamaParams]

/** GPU-side parameters for RoPE */
case class RoPEParams(
  headSize: Int32,
  maxSeqLen: Int32,
  theta: Float32,
  position: Int32,  // Current position in sequence
) extends GStruct[RoPEParams]

/** GPU-side parameters for flash attention */
case class FlashAttnParams(
  B: Int32,       // Batch size
  T: Int32,       // Current sequence length
  maxT: Int32,    // Maximum sequence length (for KV cache)
  NH: Int32,      // Number of query heads
  NKV: Int32,     // Number of KV heads
  HS: Int32,      // Head size
  scale: Float32, // 1/sqrt(head_size)
) extends GStruct[FlashAttnParams]
