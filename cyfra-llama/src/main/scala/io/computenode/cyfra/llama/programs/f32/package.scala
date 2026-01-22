package io.computenode.cyfra.llama.programs

/** F32 GPU programs for Llama inference.
  *
  * Programs for transformer operations using Float32 precision:
  *   - EmbeddingProgram: Token embedding lookup
  *   - RMSNormProgram: Root mean square normalization
  *   - RoPEProgram: Rotary position embeddings
  *   - TiledMatmulVecProgram: Matrix-vector multiplication with subgroup reduction
  *   - ResidualAddProgram/CopyProgram: Residual connections and data copying
  *   - SwiGLUProgram: SwiGLU activation
  *   - Q4KMatmulVecProgram/Q6KMatmulVecProgram: Quantized matmul
  *   - KVCacheWriteK/KVCacheWriteV: KV cache write programs
  *   - KVCachedAttention: Cached attention computation
  */
package object f32:
  /** Alias for layered Q4K matmul program */
  val Q4KMatmulLayered = Q4KMatmulVecProgram.Layered
  
  /** Alias for layered Q6K matmul program */
  val Q6KMatmulLayered = Q6KMatmulVecProgram.Layered
