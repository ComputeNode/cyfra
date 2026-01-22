package io.computenode.cyfra.llm

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llm.programs.*
import org.lwjgl.BufferUtils

import java.nio.{ByteBuffer, ByteOrder, FloatBuffer}
import java.io.{DataInputStream, FileInputStream}
import scala.compiletime.uninitialized

/** GPT-2 Model implementation in Cyfra.
  *
  * This is a port of karpathy/llm.c to GPU-accelerated Scala using Cyfra DSL.
  * The model supports forward pass, backward pass, and training with AdamW.
  */
class GPT2Model(val config: GPT2Config)(using runtime: CyfraRuntime):

  // Parameter buffers (CPU side for now, will be transferred to GPU)
  private var paramsMemory: Array[Float] = uninitialized
  private var gradsMemory: Array[Float] = uninitialized
  private var mMemory: Array[Float] = uninitialized
  private var vMemory: Array[Float] = uninitialized

  // Activation buffers
  private var actsMemory: Array[Float] = uninitialized
  private var gradsActsMemory: Array[Float] = uninitialized

  // Runtime state
  private var batchSize: Int = 0
  private var seqLen: Int = 0
  private var inputs: Array[Int] = uninitialized
  private var targets: Array[Int] = uninitialized
  private var meanLoss: Float = -1.0f

  // Parameter offsets (cumulative sums of param sizes)
  private val paramOffsets: Array[Int] = {
    val sizes = config.parameterSizes
    val offsets = new Array[Int](sizes.length + 1)
    offsets(0) = 0
    for i <- sizes.indices do offsets(i + 1) = offsets(i) + sizes(i)
    offsets
  }

  /** Load model from checkpoint file (llm.c format) */
  def loadFromCheckpoint(path: String): Unit =
    val fis = new FileInputStream(path)
    val dis = new DataInputStream(fis)

    // Read header (256 ints)
    val header = new Array[Int](256)
    for i <- header.indices do
      // Read as little-endian
      val b = new Array[Byte](4)
      dis.readFully(b)
      header(i) = ByteBuffer.wrap(b).order(ByteOrder.LITTLE_ENDIAN).getInt

    // Validate magic number and version
    require(header(0) == 20240326, s"Bad magic number: ${header(0)}")
    require(header(1) == 3, s"Bad version: ${header(1)}")

    // Verify config matches
    require(header(2) == config.maxSeqLen, s"max_seq_len mismatch: ${header(2)} vs ${config.maxSeqLen}")
    require(header(3) == config.vocabSize, s"vocab_size mismatch: ${header(3)} vs ${config.vocabSize}")
    require(header(4) == config.numLayers, s"num_layers mismatch: ${header(4)} vs ${config.numLayers}")
    require(header(5) == config.numHeads, s"num_heads mismatch: ${header(5)} vs ${config.numHeads}")
    require(header(6) == config.channels, s"channels mismatch: ${header(6)} vs ${config.channels}")
    require(header(7) == config.paddedVocabSize, s"padded_vocab_size mismatch: ${header(7)} vs ${config.paddedVocabSize}")

    // Allocate and read parameters
    paramsMemory = new Array[Float](config.numParameters)
    val paramBytes = new Array[Byte](config.numParameters * 4)
    dis.readFully(paramBytes)

    // Convert bytes to floats (little-endian)
    val bb = ByteBuffer.wrap(paramBytes).order(ByteOrder.LITTLE_ENDIAN)
    bb.asFloatBuffer().get(paramsMemory)

    dis.close()

    // Initialize other buffers to null (lazy allocation)
    gradsMemory = null
    mMemory = null
    vMemory = null
    actsMemory = null
    gradsActsMemory = null

  /** Initialize random parameters for training from scratch */
  def initRandomParams(seed: Long = 42L): Unit =
    val rng = new scala.util.Random(seed)
    paramsMemory = new Array[Float](config.numParameters)

    // Xavier initialization
    val C = config.channels
    for i <- paramsMemory.indices do
      paramsMemory(i) = (rng.nextGaussian() * 0.02).toFloat

    gradsMemory = null
    mMemory = null
    vMemory = null
    actsMemory = null
    gradsActsMemory = null

  /** Get parameter slice by index */
  def getParam(idx: Int): Array[Float] =
    val start = paramOffsets(idx)
    val end = paramOffsets(idx + 1)
    paramsMemory.slice(start, end)

  // Parameter accessors
  def wte: Array[Float] = getParam(0)   // Token embeddings (Vp, C)
  def wpe: Array[Float] = getParam(1)   // Position embeddings (maxT, C)
  def ln1w: Array[Float] = getParam(2)  // LayerNorm 1 weights (L, C)
  def ln1b: Array[Float] = getParam(3)  // LayerNorm 1 biases (L, C)
  def qkvw: Array[Float] = getParam(4)  // QKV projection weights (L, 3*C, C)
  def qkvb: Array[Float] = getParam(5)  // QKV projection biases (L, 3*C)
  def attprojw: Array[Float] = getParam(6) // Attention projection weights (L, C, C)
  def attprojb: Array[Float] = getParam(7) // Attention projection biases (L, C)
  def ln2w: Array[Float] = getParam(8)  // LayerNorm 2 weights (L, C)
  def ln2b: Array[Float] = getParam(9)  // LayerNorm 2 biases (L, C)
  def fcw: Array[Float] = getParam(10)  // MLP fc weights (L, 4*C, C)
  def fcb: Array[Float] = getParam(11)  // MLP fc biases (L, 4*C)
  def fcprojw: Array[Float] = getParam(12) // MLP projection weights (L, C, 4*C)
  def fcprojb: Array[Float] = getParam(13) // MLP projection biases (L, C)
  def lnfw: Array[Float] = getParam(14) // Final LayerNorm weights (C)
  def lnfb: Array[Float] = getParam(15) // Final LayerNorm biases (C)

  /** CPU-side forward pass (reference implementation).
    *
    * This implements the forward pass on CPU for correctness testing.
    * The GPU version will use GExecution pipelines.
    */
  def forwardCPU(inputTokens: Array[Int], targetTokens: Array[Int] = null, B: Int, T: Int): Float =
    require(paramsMemory != null, "Model not initialized - call loadFromCheckpoint or initRandomParams first")
    require(inputTokens.length == B * T, s"Input size mismatch: ${inputTokens.length} vs ${B * T}")

    // Allocate activations lazily
    if actsMemory == null || batchSize != B || seqLen != T then
      batchSize = B
      seqLen = T
      actsMemory = new Array[Float](config.numActivations(B, T))
      inputs = new Array[Int](B * T)
      targets = new Array[Int](B * T)

    // Cache inputs/targets
    System.arraycopy(inputTokens, 0, inputs, 0, B * T)
    if targetTokens != null then System.arraycopy(targetTokens, 0, targets, 0, B * T)

    val C = config.channels
    val V = config.vocabSize
    val Vp = config.paddedVocabSize
    val L = config.numLayers
    val NH = config.numHeads

    // Activation offsets (using config.activationSizes)
    val actSizes = config.activationSizes(B, T)
    val actOffsets = new Array[Int](actSizes.length + 1)
    actOffsets(0) = 0
    for i <- actSizes.indices do actOffsets(i + 1) = actOffsets(i) + actSizes(i)

    // Helper to get activation slice
    def getAct(idx: Int): Array[Float] =
      val start = actOffsets(idx)
      val end = actOffsets(idx + 1)
      actsMemory.slice(start, end)

    // Activation indices (matching llm.c)
    val encoded = getAct(0)     // (B, T, C)
    val ln1 = getAct(1)         // (L, B, T, C)
    val ln1Mean = getAct(2)     // (L, B, T)
    val ln1Rstd = getAct(3)     // (L, B, T)
    val qkv = getAct(4)         // (L, B, T, 3*C)
    val atty = getAct(5)        // (L, B, T, C)
    val preatt = getAct(6)      // (L, B, NH, T, T)
    val att = getAct(7)         // (L, B, NH, T, T)
    val attproj = getAct(8)     // (L, B, T, C)
    val residual2 = getAct(9)   // (L, B, T, C)
    val ln2 = getAct(10)        // (L, B, T, C)
    val ln2Mean = getAct(11)    // (L, B, T)
    val ln2Rstd = getAct(12)    // (L, B, T)
    val fch = getAct(13)        // (L, B, T, 4*C)
    val fchGelu = getAct(14)    // (L, B, T, 4*C)
    val fcproj = getAct(15)     // (L, B, T, C)
    val residual3 = getAct(16)  // (L, B, T, C)
    val lnf = getAct(17)        // (B, T, C)
    val lnfMean = getAct(18)    // (B, T)
    val lnfRstd = getAct(19)    // (B, T)
    val logits = getAct(20)     // (B, T, Vp)
    val probs = getAct(21)      // (B, T, Vp)
    val losses = getAct(22)     // (B, T)

    // Forward pass

    // 1. Encoder: out = wte[inp] + wpe[pos]
    encoderForward(encoded, inputs, wte, wpe, B, T, C)

    // 2. Transformer layers
    var residual = encoded
    for l <- 0 until L do
      val residualIn = if l == 0 then encoded else residual3.slice((l - 1) * B * T * C, l * B * T * C)

      // Get layer-specific parameters
      val lLn1w = ln1w.slice(l * C, (l + 1) * C)
      val lLn1b = ln1b.slice(l * C, (l + 1) * C)
      val lQkvw = qkvw.slice(l * 3 * C * C, (l + 1) * 3 * C * C)
      val lQkvb = qkvb.slice(l * 3 * C, (l + 1) * 3 * C)
      val lAttprojw = attprojw.slice(l * C * C, (l + 1) * C * C)
      val lAttprojb = attprojb.slice(l * C, (l + 1) * C)
      val lLn2w = ln2w.slice(l * C, (l + 1) * C)
      val lLn2b = ln2b.slice(l * C, (l + 1) * C)
      val lFcw = fcw.slice(l * 4 * C * C, (l + 1) * 4 * C * C)
      val lFcb = fcb.slice(l * 4 * C, (l + 1) * 4 * C)
      val lFcprojw = fcprojw.slice(l * C * 4 * C, (l + 1) * C * 4 * C)
      val lFcprojb = fcprojb.slice(l * C, (l + 1) * C)

      // Get layer-specific activations
      val lLn1 = ln1.slice(l * B * T * C, (l + 1) * B * T * C)
      val lLn1Mean = ln1Mean.slice(l * B * T, (l + 1) * B * T)
      val lLn1Rstd = ln1Rstd.slice(l * B * T, (l + 1) * B * T)
      val lQkv = qkv.slice(l * B * T * 3 * C, (l + 1) * B * T * 3 * C)
      val lAtty = atty.slice(l * B * T * C, (l + 1) * B * T * C)
      val lPreatt = preatt.slice(l * B * NH * T * T, (l + 1) * B * NH * T * T)
      val lAtt = att.slice(l * B * NH * T * T, (l + 1) * B * NH * T * T)
      val lAttproj = attproj.slice(l * B * T * C, (l + 1) * B * T * C)
      val lResidual2 = residual2.slice(l * B * T * C, (l + 1) * B * T * C)
      val lLn2 = ln2.slice(l * B * T * C, (l + 1) * B * T * C)
      val lLn2Mean = ln2Mean.slice(l * B * T, (l + 1) * B * T)
      val lLn2Rstd = ln2Rstd.slice(l * B * T, (l + 1) * B * T)
      val lFch = fch.slice(l * B * T * 4 * C, (l + 1) * B * T * 4 * C)
      val lFchGelu = fchGelu.slice(l * B * T * 4 * C, (l + 1) * B * T * 4 * C)
      val lFcproj = fcproj.slice(l * B * T * C, (l + 1) * B * T * C)
      val lResidual3 = residual3.slice(l * B * T * C, (l + 1) * B * T * C)

      // LayerNorm 1
      layernormForward(lLn1, lLn1Mean, lLn1Rstd, residualIn, lLn1w, lLn1b, B, T, C)

      // QKV projection
      matmulForward(lQkv, lLn1, lQkvw, lQkvb, B, T, C, 3 * C)

      // Self-attention
      attentionForward(lAtty, lPreatt, lAtt, lQkv, B, T, C, NH)

      // Attention projection
      matmulForward(lAttproj, lAtty, lAttprojw, lAttprojb, B, T, C, C)

      // Residual 1
      residualForward(lResidual2, residualIn, lAttproj, B * T * C)

      // LayerNorm 2
      layernormForward(lLn2, lLn2Mean, lLn2Rstd, lResidual2, lLn2w, lLn2b, B, T, C)

      // MLP fc
      matmulForward(lFch, lLn2, lFcw, lFcb, B, T, C, 4 * C)

      // GELU
      geluForward(lFchGelu, lFch, B * T * 4 * C)

      // MLP projection
      matmulForward(lFcproj, lFchGelu, lFcprojw, lFcprojb, B, T, 4 * C, C)

      // Residual 2
      residualForward(lResidual3, lResidual2, lFcproj, B * T * C)

      residual = lResidual3

    // 3. Final LayerNorm
    val finalResidual = residual3.slice((L - 1) * B * T * C, L * B * T * C)
    layernormForward(lnf, lnfMean, lnfRstd, finalResidual, lnfw, lnfb, B, T, C)

    // 4. Output projection (reusing wte for weight tying)
    matmulForward(logits, lnf, wte, null, B, T, C, Vp)

    // 5. Softmax
    softmaxForward(probs, logits, B, T, V, Vp)

    // 6. Cross-entropy loss (if targets provided)
    if targetTokens != null then
      crossentropyForward(losses, probs, targets, B, T, Vp)
      meanLoss = losses.sum / (B * T)
      meanLoss
    else
      meanLoss = -1.0f
      -1.0f

  /** Get the current mean loss */
  def getMeanLoss: Float = meanLoss

  /** Get logits from the last forward pass */
  def getLogits(B: Int, T: Int): Array[Float] =
    val actSizes = config.activationSizes(B, T)
    val actOffsets = new Array[Int](actSizes.length + 1)
    actOffsets(0) = 0
    for i <- actSizes.indices do actOffsets(i + 1) = actOffsets(i) + actSizes(i)
    actsMemory.slice(actOffsets(20), actOffsets(21))

  /** Get probabilities from the last forward pass */
  def getProbs(B: Int, T: Int): Array[Float] =
    val actSizes = config.activationSizes(B, T)
    val actOffsets = new Array[Int](actSizes.length + 1)
    actOffsets(0) = 0
    for i <- actSizes.indices do actOffsets(i + 1) = actOffsets(i) + actSizes(i)
    actsMemory.slice(actOffsets(21), actOffsets(22))

  // ============= CPU Reference Implementations =============

  private def encoderForward(out: Array[Float], inp: Array[Int], wte: Array[Float], wpe: Array[Float], B: Int, T: Int, C: Int): Unit =
    for
      b <- 0 until B
      t <- 0 until T
    do
      val outOffset = b * T * C + t * C
      val ix = inp(b * T + t)
      val wteOffset = ix * C
      val wpeOffset = t * C
      for i <- 0 until C do
        out(outOffset + i) = wte(wteOffset + i) + wpe(wpeOffset + i)

  private def layernormForward(out: Array[Float], mean: Array[Float], rstd: Array[Float],
                               inp: Array[Float], weight: Array[Float], bias: Array[Float],
                               B: Int, T: Int, C: Int): Unit =
    val eps = 1e-5f
    for
      b <- 0 until B
      t <- 0 until T
    do
      val offset = b * T * C + t * C

      // Calculate mean
      var m = 0.0f
      for i <- 0 until C do m += inp(offset + i)
      m = m / C

      // Calculate variance
      var v = 0.0f
      for i <- 0 until C do
        val xshift = inp(offset + i) - m
        v += xshift * xshift
      v = v / C

      // Calculate rstd
      val s = 1.0f / math.sqrt(v + eps).toFloat

      // Normalize, scale and shift
      for i <- 0 until C do
        val n = (inp(offset + i) - m) * s
        out(offset + i) = n * weight(i) + bias(i)

      mean(b * T + t) = m
      rstd(b * T + t) = s

  private def matmulForward(out: Array[Float], inp: Array[Float], weight: Array[Float], bias: Array[Float],
                            B: Int, T: Int, C: Int, OC: Int): Unit =
    for
      b <- 0 until B
      t <- 0 until T
      o <- 0 until OC
    do
      var sum = if bias != null then bias(o) else 0.0f
      for i <- 0 until C do
        sum += inp(b * T * C + t * C + i) * weight(o * C + i)
      out(b * T * OC + t * OC + o) = sum

  private def attentionForward(out: Array[Float], preatt: Array[Float], att: Array[Float],
                               inp: Array[Float], B: Int, T: Int, C: Int, NH: Int): Unit =
    val C3 = C * 3
    val hs = C / NH
    val scale = 1.0f / math.sqrt(hs).toFloat

    for
      b <- 0 until B
      t <- 0 until T
      h <- 0 until NH
    do
      val queryOffset = b * T * C3 + t * C3 + h * hs
      val preattOffset = b * NH * T * T + h * T * T + t * T
      val attOffset = preattOffset

      // Pass 1: Q @ K and find max
      var maxval = -10000.0f
      for t2 <- 0 to t do
        val keyOffset = b * T * C3 + t2 * C3 + h * hs + C
        var dotprod = 0.0f
        for i <- 0 until hs do
          dotprod += inp(queryOffset + i) * inp(keyOffset + i)
        dotprod *= scale
        preatt(preattOffset + t2) = dotprod
        if dotprod > maxval then maxval = dotprod

      // Pass 2: exp and sum
      var expsum = 0.0f
      for t2 <- 0 to t do
        val expv = math.exp(preatt(preattOffset + t2) - maxval).toFloat
        att(attOffset + t2) = expv
        expsum += expv

      val expsumInv = if expsum == 0.0f then 0.0f else 1.0f / expsum

      // Pass 3: normalize
      for t2 <- 0 until T do
        if t2 <= t then att(attOffset + t2) *= expsumInv
        else att(attOffset + t2) = 0.0f

      // Pass 4: accumulate weighted values
      val outOffset = b * T * C + t * C + h * hs
      for i <- 0 until hs do out(outOffset + i) = 0.0f
      for t2 <- 0 to t do
        val valueOffset = b * T * C3 + t2 * C3 + h * hs + C * 2
        val attWeight = att(attOffset + t2)
        for i <- 0 until hs do
          out(outOffset + i) += attWeight * inp(valueOffset + i)

  private def geluForward(out: Array[Float], inp: Array[Float], N: Int): Unit =
    val scalingFactor = math.sqrt(2.0 / math.Pi).toFloat
    for i <- 0 until N do
      val x = inp(i)
      val cube = 0.044715f * x * x * x
      out(i) = 0.5f * x * (1.0f + math.tanh(scalingFactor * (x + cube)).toFloat)

  private def residualForward(out: Array[Float], inp1: Array[Float], inp2: Array[Float], N: Int): Unit =
    for i <- 0 until N do out(i) = inp1(i) + inp2(i)

  private def softmaxForward(probs: Array[Float], logits: Array[Float], B: Int, T: Int, V: Int, Vp: Int): Unit =
    for
      b <- 0 until B
      t <- 0 until T
    do
      val offset = b * T * Vp + t * Vp

      // Find max
      var maxval = -10000.0f
      for i <- 0 until V do
        if logits(offset + i) > maxval then maxval = logits(offset + i)

      // Exp and sum
      var sum = 0.0f
      for i <- 0 until V do
        probs(offset + i) = math.exp(logits(offset + i) - maxval).toFloat
        sum += probs(offset + i)

      // Normalize
      for i <- 0 until V do probs(offset + i) /= sum
      for i <- V until Vp do probs(offset + i) = 0.0f

  private def crossentropyForward(losses: Array[Float], probs: Array[Float], targets: Array[Int],
                                  B: Int, T: Int, Vp: Int): Unit =
    for
      b <- 0 until B
      t <- 0 until T
    do
      val ix = targets(b * T + t)
      losses(b * T + t) = -math.log(probs(b * T * Vp + t * Vp + ix)).toFloat

object GPT2Model:
  /** Load GPT-2 model from checkpoint */
  def fromCheckpoint(path: String, config: GPT2Config = GPT2Config.GPT2_124M)(using CyfraRuntime): GPT2Model =
    val model = new GPT2Model(config)
    model.loadFromCheckpoint(path)
    model

  /** Create new GPT-2 model with random initialization */
  def random(config: GPT2Config = GPT2Config.GPT2_124M, seed: Long = 42L)(using CyfraRuntime): GPT2Model =
    val model = new GPT2Model(config)
    model.initRandomParams(seed)
    model
