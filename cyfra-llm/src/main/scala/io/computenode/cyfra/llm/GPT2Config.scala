package io.computenode.cyfra.llm

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.struct.GStruct

/** GPT-2 model configuration.
  *
  * Mirrors the GPT2Config struct from llm.c:
  *   - max_seq_len: maximum sequence length (e.g., 1024)
  *   - vocab_size: vocabulary size (e.g., 50257)
  *   - padded_vocab_size: vocab size padded to %128==0 (e.g., 50304)
  *   - num_layers: number of transformer layers (e.g., 12)
  *   - num_heads: number of attention heads (e.g., 12)
  *   - channels: embedding dimension (e.g., 768)
  */
case class GPT2Config(
  maxSeqLen: Int,
  vocabSize: Int,
  paddedVocabSize: Int,
  numLayers: Int,
  numHeads: Int,
  channels: Int,
):
  def headSize: Int = channels / numHeads

  /** Calculate parameter sizes for all 16 parameter tensors */
  def parameterSizes: Array[Int] = Array(
    paddedVocabSize * channels,   // wte: token embeddings (V, C)
    maxSeqLen * channels,         // wpe: position embeddings (maxT, C)
    numLayers * channels,         // ln1w: layer norm 1 weights (L, C)
    numLayers * channels,         // ln1b: layer norm 1 biases (L, C)
    numLayers * 3 * channels * channels, // qkvw: QKV projection weights (L, 3*C, C)
    numLayers * 3 * channels,     // qkvb: QKV projection biases (L, 3*C)
    numLayers * channels * channels, // attprojw: attention projection weights (L, C, C)
    numLayers * channels,         // attprojb: attention projection biases (L, C)
    numLayers * channels,         // ln2w: layer norm 2 weights (L, C)
    numLayers * channels,         // ln2b: layer norm 2 biases (L, C)
    numLayers * 4 * channels * channels, // fcw: MLP fc weights (L, 4*C, C)
    numLayers * 4 * channels,     // fcb: MLP fc biases (L, 4*C)
    numLayers * channels * 4 * channels, // fcprojw: MLP projection weights (L, C, 4*C)
    numLayers * channels,         // fcprojb: MLP projection biases (L, C)
    channels,                     // lnfw: final layer norm weights (C)
    channels,                     // lnfb: final layer norm biases (C)
  )

  def numParameters: Int = parameterSizes.sum

  /** Calculate activation sizes for batch size B and sequence length T */
  def activationSizes(B: Int, T: Int): Array[Int] = Array(
    B * T * channels,             // encoded
    numLayers * B * T * channels, // ln1
    numLayers * B * T,            // ln1_mean
    numLayers * B * T,            // ln1_rstd
    numLayers * B * T * 3 * channels, // qkv
    numLayers * B * T * channels, // atty
    numLayers * B * numHeads * T * T, // preatt
    numLayers * B * numHeads * T * T, // att
    numLayers * B * T * channels, // attproj
    numLayers * B * T * channels, // residual2
    numLayers * B * T * channels, // ln2
    numLayers * B * T,            // ln2_mean
    numLayers * B * T,            // ln2_rstd
    numLayers * B * T * 4 * channels, // fch
    numLayers * B * T * 4 * channels, // fch_gelu
    numLayers * B * T * channels, // fcproj
    numLayers * B * T * channels, // residual3
    B * T * channels,             // lnf
    B * T,                        // lnf_mean
    B * T,                        // lnf_rstd
    B * T * paddedVocabSize,      // logits
    B * T * paddedVocabSize,      // probs
    B * T,                        // losses
  )

  def numActivations(B: Int, T: Int): Int = activationSizes(B, T).sum

object GPT2Config:
  /** Standard GPT-2 124M configuration */
  val GPT2_124M: GPT2Config = GPT2Config(
    maxSeqLen = 1024,
    vocabSize = 50257,
    paddedVocabSize = 50304,
    numLayers = 12,
    numHeads = 12,
    channels = 768,
  )

  /** GPT-2 350M configuration */
  val GPT2_350M: GPT2Config = GPT2Config(
    maxSeqLen = 1024,
    vocabSize = 50257,
    paddedVocabSize = 50304,
    numLayers = 24,
    numHeads = 16,
    channels = 1024,
  )

  /** GPT-2 774M configuration */
  val GPT2_774M: GPT2Config = GPT2Config(
    maxSeqLen = 1024,
    vocabSize = 50257,
    paddedVocabSize = 50304,
    numLayers = 36,
    numHeads = 20,
    channels = 1280,
  )

  /** GPT-2 1558M (1.5B) configuration */
  val GPT2_1558M: GPT2Config = GPT2Config(
    maxSeqLen = 1024,
    vocabSize = 50257,
    paddedVocabSize = 50304,
    numLayers = 48,
    numHeads = 25,
    channels = 1600,
  )

/** GPU-side parameters for layer operations */
case class LayerParams(
  B: Int32,      // batch size
  T: Int32,      // sequence length
  C: Int32,      // channels
  NH: Int32,     // number of heads
  V: Int32,      // vocab size
  Vp: Int32,     // padded vocab size
) extends GStruct[LayerParams]

/** GPU-side parameters for matmul */
case class MatmulParams(
  BT: Int32,     // B * T (batch * time)
  C: Int32,      // input channels
  OC: Int32,     // output channels
) extends GStruct[MatmulParams]

/** GPU-side parameters for attention */
case class AttentionParams(
  B: Int32,      // batch size
  T: Int32,      // sequence length
  C: Int32,      // channels
  NH: Int32,     // number of heads
  hs: Int32,     // head size
) extends GStruct[AttentionParams]
