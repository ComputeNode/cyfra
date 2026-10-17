package io.computenode.cyfra.llama

import io.computenode.cyfra.core.expression.types.Int32
import io.computenode.cyfra.core.expression.Value

package object programs:

  /** Runtime parameters for attention and RoPE operations.
    *
    * Passed via uniform to support single compiled pipeline with runtime-varying positions. Used by RoPE (for position encoding) and attention (for
    * KV cache operations).
    */
  case class AttentionParams(
    seqLen: Int32, // actual sequence length (startPos + T)
    startPos: Int32, // position of first query token in full sequence
  ) derives Value
