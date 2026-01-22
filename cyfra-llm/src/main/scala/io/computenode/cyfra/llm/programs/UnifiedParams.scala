package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.dsl.{*, given}

/** Unified GPU params struct for all GPT-2 programs.
  *
  * Contains all fields needed by any program. Each program reads only
  * the fields it needs. This allows sharing a single GUniform across
  * all programs in a GExecution pipeline.
  *
  * For layer-specific operations, `layer` specifies which layer's parameters
  * to use (offset = layer * paramSize).
  */
case class GPT2Params(
  B: Int32,       // batch size
  T: Int32,       // sequence length
  C: Int32,       // channels (embedding dim)
  NH: Int32,      // number of heads
  V: Int32,       // vocab size
  Vp: Int32,      // padded vocab size
  OC: Int32,      // output channels (for matmul) OR head_size for attention
  hasBias: Int32, // whether matmul uses bias (0 or 1)
  N: Int32,       // total elements (for gelu/residual)
  maxT: Int32,    // max sequence length (for encoder)
  layer: Int32,   // current layer index (for parameter offset calculation)
  L: Int32,       // total number of layers
) extends GStruct[GPT2Params]

object GPT2Params:
  /** Create params for encoder */
  def forEncoder(B: Int, T: Int, C: Int, maxT: Int): GPT2Params =
    GPT2Params(B, T, C, 1, 0, 0, 0, 0, B * T * C, maxT, 0, 1)

  /** Create params for layer norm */
  def forLayerNorm(B: Int, T: Int, C: Int, layer: Int = 0, L: Int = 1): GPT2Params =
    GPT2Params(B, T, C, 1, 0, 0, 0, 0, B * T * C, 0, layer, L)

  /** Create params for matmul */
  def forMatmul(BT: Int, C: Int, OC: Int, hasBias: Boolean, layer: Int = 0, L: Int = 1): GPT2Params =
    GPT2Params(BT, 1, C, 1, 0, 0, OC, if hasBias then 1 else 0, BT * OC, 0, layer, L)

  /** Create params for GELU */
  def forGelu(N: Int): GPT2Params =
    GPT2Params(0, 0, 0, 1, 0, 0, 0, 0, N, 0, 0, 1)

  /** Create params for residual */
  def forResidual(N: Int): GPT2Params =
    GPT2Params(0, 0, 0, 1, 0, 0, 0, 0, N, 0, 0, 1)

  /** Create params for softmax */
  def forSoftmax(B: Int, T: Int, V: Int, Vp: Int): GPT2Params =
    GPT2Params(B, T, 0, 1, V, Vp, 0, 0, B * T, 0, 0, 1)

  /** Create params for cross-entropy */
  def forCrossEntropy(B: Int, T: Int, Vp: Int): GPT2Params =
    GPT2Params(B, T, 0, 1, 0, Vp, 0, 0, B * T, 0, 0, 1)

  /** Create params for attention */
  def forAttention(B: Int, T: Int, C: Int, NH: Int, layer: Int = 0, L: Int = 1): GPT2Params =
    GPT2Params(B, T, C, NH, 0, 0, C / NH, 0, B * NH * T, 0, layer, L)  // OC stores head_size

  /** Create full pipeline params for a specific layer */
  def forLayer(B: Int, T: Int, C: Int, NH: Int, V: Int, Vp: Int, layer: Int, L: Int, maxT: Int): GPT2Params =
    GPT2Params(B, T, C, NH, V, Vp, C / NH, 1, B * T * C, maxT, layer, L)
