package io.computenode.cyfra.llama

import io.computenode.cyfra.dsl.Value.Int32
import io.computenode.cyfra.dsl.struct.GStruct

package object programs:

  /** Runtime parameters for attention and RoPE operations.
    * 
    * Passed via uniform to support single compiled pipeline with runtime-varying positions.
    * Used by RoPE (for position encoding) and attention (for KV cache operations).
    */
  case class AttentionParams(
    seqLen: Int32,    // actual sequence length (startPos + T)
    startPos: Int32,  // position of first query token in full sequence
  ) extends GStruct[AttentionParams]
