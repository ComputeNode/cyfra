package io.computenode.cyfra.llama.inference

import io.computenode.cyfra.llama.model.LlamaConfig

/** CPU-based Llama inference implementation.
  * 
  * This is extracted from LlamaInference to allow F16 pipeline to use
  * CPU inference without circular dependencies.
  */
object CPUInference:
  
  case class LayerWeights(
    attnNorm: Array[Float],
    wq: Array[Float],
    wk: Array[Float],
    wv: Array[Float],
    wo: Array[Float],
    ffnNorm: Array[Float],
    ffnGate: Array[Float],
    ffnUp: Array[Float],
    ffnDown: Array[Float],
  )
  
  /** Run forward pass on CPU. Returns logits for the last token position. */
  def forwardCPU(
    tokens: Array[Int],
    tokenEmbed: Array[Float],
    layers: Seq[LayerWeights],
    outputNorm: Array[Float],
    output: Array[Float],
    config: LlamaConfig,
  ): Array[Float] =
    val T = tokens.length
    val C = config.hiddenSize
    val NH = config.numAttentionHeads
    val NKV = config.numKeyValueHeads
    val HS = config.headSize
    val FFN = config.intermediateSize
    val V = config.vocabSize
    
    // Embedding lookup
    var hidden = Array.ofDim[Float](T * C)
    for t <- 0 until T do
      val tokenId = tokens(t)
      System.arraycopy(tokenEmbed, tokenId * C, hidden, t * C, C)
    
    // Process each layer
    for layer <- layers do
      val residual = hidden.clone()
      
      // Attention RMSNorm
      hidden = rmsNorm(hidden, layer.attnNorm, T, C, config.rmsNormEps)
      
      // Q, K, V projections
      val q = matmul(hidden, layer.wq, T, C, C)
      val k = matmul(hidden, layer.wk, T, C, NKV * HS)
      val v = matmul(hidden, layer.wv, T, C, NKV * HS)
      
      // RoPE
      applyRoPE(q, T, NH, HS, config.ropeTheta)
      applyRoPE(k, T, NKV, HS, config.ropeTheta)
      
      // Attention
      val attnOut = attention(q, k, v, T, NH, NKV, HS)
      
      // Output projection
      val attnProj = matmul(attnOut, layer.wo, T, C, C)
      
      // Residual connection
      for i <- 0 until T * C do
        hidden(i) = residual(i) + attnProj(i)
      
      val residual2 = hidden.clone()
      
      // FFN RMSNorm
      hidden = rmsNorm(hidden, layer.ffnNorm, T, C, config.rmsNormEps)
      
      // FFN
      val gate = matmul(hidden, layer.ffnGate, T, C, FFN)
      val up = matmul(hidden, layer.ffnUp, T, C, FFN)
      
      // SwiGLU activation
      val ffnHidden = Array.ofDim[Float](T * FFN)
      for i <- 0 until T * FFN do
        val g = gate(i)
        val u = up(i)
        val silu = g / (1.0f + math.exp(-g).toFloat)
        ffnHidden(i) = silu * u
      
      // FFN down projection
      val ffnOut = matmul(ffnHidden, layer.ffnDown, T, FFN, C)
      
      // Residual connection
      for i <- 0 until T * C do
        hidden(i) = residual2(i) + ffnOut(i)
    
    // Final layer norm
    hidden = rmsNorm(hidden, outputNorm, T, C, config.rmsNormEps)
    
    // Output projection - only last token
    val lastHidden = hidden.slice((T - 1) * C, T * C)
    val logits = Array.ofDim[Float](V)
    for i <- 0 until V do
      var sum = 0.0f
      for j <- 0 until C do
        sum += lastHidden(j) * output(i * C + j)
      logits(i) = sum
    
    logits
  
  /** RMS normalization. */
  private def rmsNorm(
    input: Array[Float],
    weight: Array[Float],
    numRows: Int,
    rowSize: Int,
    eps: Double,
  ): Array[Float] =
    val output = Array.ofDim[Float](input.length)
    for row <- 0 until numRows do
      val offset = row * rowSize
      
      // Compute RMS
      var sumSq = 0.0
      for i <- 0 until rowSize do
        val x = input(offset + i)
        sumSq += x * x
      val rms = math.sqrt(sumSq / rowSize + eps)
      val scale = 1.0 / rms
      
      // Normalize and apply weight
      for i <- 0 until rowSize do
        output(offset + i) = (input(offset + i) * scale * weight(i)).toFloat
    
    output
  
  /** Matrix multiplication: output = input @ weight. */
  private def matmul(
    input: Array[Float],
    weight: Array[Float],
    batchSize: Int,
    inFeatures: Int,
    outFeatures: Int,
  ): Array[Float] =
    val output = Array.ofDim[Float](batchSize * outFeatures)
    for b <- 0 until batchSize do
      for i <- 0 until outFeatures do
        var sum = 0.0f
        for j <- 0 until inFeatures do
          sum += input(b * inFeatures + j) * weight(i * inFeatures + j)
        output(b * outFeatures + i) = sum
    output
  
  /** Apply rotary position embeddings (RoPE). */
  private def applyRoPE(
    tensor: Array[Float],
    seqLen: Int,
    numHeads: Int,
    headSize: Int,
    theta: Double,
  ): Unit =
    for pos <- 0 until seqLen do
      for head <- 0 until numHeads do
        val offset = pos * numHeads * headSize + head * headSize
        var i = 0
        while i < headSize do
          val freq = 1.0 / math.pow(theta, (2 * (i / 2)).toDouble / headSize)
          val angle = pos * freq
          val cosA = math.cos(angle).toFloat
          val sinA = math.sin(angle).toFloat
          
          val x = tensor(offset + i)
          val y = tensor(offset + i + 1)
          
          tensor(offset + i) = x * cosA - y * sinA
          tensor(offset + i + 1) = x * sinA + y * cosA
          
          i += 2
  
  /** Multi-head attention with grouped-query attention (GQA). */
  private def attention(
    q: Array[Float],
    k: Array[Float],
    v: Array[Float],
    seqLen: Int,
    numHeads: Int,
    numKvHeads: Int,
    headSize: Int,
  ): Array[Float] =
    val output = Array.ofDim[Float](seqLen * numHeads * headSize)
    val scale = 1.0f / math.sqrt(headSize).toFloat
    val gqaRatio = numHeads / numKvHeads
    
    for pos <- 0 until seqLen do
      for head <- 0 until numHeads do
        val kvHead = head / gqaRatio
        val qOffset = pos * numHeads * headSize + head * headSize
        val outOffset = qOffset
        
        // Compute attention weights for all positions
        val scores = Array.ofDim[Float](pos + 1)
        for kPos <- 0 to pos do
          val kOffset = kPos * numKvHeads * headSize + kvHead * headSize
          
          // Dot product
          var sum = 0.0f
          for d <- 0 until headSize do
            sum += q(qOffset + d) * k(kOffset + d)
          
          scores(kPos) = sum * scale
        
        // Softmax
        val maxScore = scores.max
        var sumExp = 0.0f
        for i <- 0 until scores.length do
          scores(i) = math.exp(scores(i) - maxScore).toFloat
          sumExp += scores(i)
        
        for i <- 0 until scores.length do
          scores(i) /= sumExp
        
        // Weighted sum of values
        for d <- 0 until headSize do
          var sum = 0.0f
          for kPos <- 0 to pos do
            val vOffset = kPos * numKvHeads * headSize + kvHead * headSize
            sum += scores(kPos) * v(vOffset + d)
          output(outOffset + d) = sum
    
    output

end CPUInference
