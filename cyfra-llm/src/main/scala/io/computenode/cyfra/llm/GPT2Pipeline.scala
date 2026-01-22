package io.computenode.cyfra.llm

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llm.programs.*

/** GPU Pipeline for GPT-2 forward pass using GExecution.
  *
  * All programs share a unified GPT2Params struct, enabling a single GUniform
  * to be passed through the execution pipeline.
  */
object GPT2Pipeline:

  // ============= Pre-Attention Pipeline =============

  case class PreAttParams(B: Int, T: Int, C: Int)

  case class PreAttLayout(
    residualIn: GBuffer[Float32],
    ln1w: GBuffer[Float32],
    ln1b: GBuffer[Float32],
    qkvw: GBuffer[Float32],
    qkvb: GBuffer[Float32],
    ln1Out: GBuffer[Float32],
    ln1Mean: GBuffer[Float32],
    ln1Rstd: GBuffer[Float32],
    qkvOut: GBuffer[Float32],
    params: GUniform[GPT2Params],
  ) derives Layout

  def buildPreAttPipeline(B: Int, T: Int, C: Int): GExecution[PreAttParams, PreAttLayout, PreAttLayout] =
    val lnSizes = LayerNormProgram.LayerNormSizes(B, T, C)
    val qkvSizes = MatmulProgram.MatmulSizes(B * T, C, 3 * C, hasBias = true)

    GExecution[PreAttParams, PreAttLayout]()
      .addProgram(LayerNormProgram.forward(lnSizes))(
        _ => lnSizes,
        l => LayerNormProgram.LayerNormForwardLayout(l.ln1Out, l.ln1Mean, l.ln1Rstd, l.residualIn, l.ln1w, l.ln1b, l.params),
      )
      .addProgram(MatmulProgram.forward(qkvSizes))(
        _ => qkvSizes,
        l => MatmulProgram.MatmulForwardLayout(l.qkvOut, l.ln1Out, l.qkvw, l.qkvb, l.params),
      )

  // ============= Post-Attention Pipeline =============

  case class PostAttParams(B: Int, T: Int, C: Int)

  case class PostAttLayout(
    residualIn: GBuffer[Float32],
    attyOut: GBuffer[Float32],
    attprojw: GBuffer[Float32],
    attprojb: GBuffer[Float32],
    ln2w: GBuffer[Float32],
    ln2b: GBuffer[Float32],
    fcw: GBuffer[Float32],
    fcb: GBuffer[Float32],
    fcprojw: GBuffer[Float32],
    fcprojb: GBuffer[Float32],
    attprojOut: GBuffer[Float32],
    residual2Out: GBuffer[Float32],
    ln2Out: GBuffer[Float32],
    ln2Mean: GBuffer[Float32],
    ln2Rstd: GBuffer[Float32],
    fchOut: GBuffer[Float32],
    fchGeluOut: GBuffer[Float32],
    fcprojOut: GBuffer[Float32],
    residual3Out: GBuffer[Float32],
    params: GUniform[GPT2Params],
  ) derives Layout

  def buildPostAttPipeline(B: Int, T: Int, C: Int): GExecution[PostAttParams, PostAttLayout, PostAttLayout] =
    val attprojSizes = MatmulProgram.MatmulSizes(B * T, C, C, hasBias = true)
    val resSizes = ResidualProgram.ResidualSizes(B * T * C)
    val lnSizes = LayerNormProgram.LayerNormSizes(B, T, C)
    val fcSizes = MatmulProgram.MatmulSizes(B * T, C, 4 * C, hasBias = true)
    val geluSizes = GeLUProgram.GeLUSizes(B * T * 4 * C)
    val fcprojSizes = MatmulProgram.MatmulSizes(B * T, 4 * C, C, hasBias = true)

    GExecution[PostAttParams, PostAttLayout]()
      .addProgram(MatmulProgram.forward(attprojSizes))(
        _ => attprojSizes,
        l => MatmulProgram.MatmulForwardLayout(l.attprojOut, l.attyOut, l.attprojw, l.attprojb, l.params),
      )
      .addProgram(ResidualProgram.forward(resSizes))(
        _ => resSizes,
        l => ResidualProgram.ResidualForwardLayout(l.residual2Out, l.residualIn, l.attprojOut, l.params),
      )
      .addProgram(LayerNormProgram.forward(lnSizes))(
        _ => lnSizes,
        l => LayerNormProgram.LayerNormForwardLayout(l.ln2Out, l.ln2Mean, l.ln2Rstd, l.residual2Out, l.ln2w, l.ln2b, l.params),
      )
      .addProgram(MatmulProgram.forward(fcSizes))(
        _ => fcSizes,
        l => MatmulProgram.MatmulForwardLayout(l.fchOut, l.ln2Out, l.fcw, l.fcb, l.params),
      )
      .addProgram(GeLUProgram.forward(geluSizes))(
        _ => geluSizes,
        l => GeLUProgram.GeLUForwardLayout(l.fchGeluOut, l.fchOut, l.params),
      )
      .addProgram(MatmulProgram.forward(fcprojSizes))(
        _ => fcprojSizes,
        l => MatmulProgram.MatmulForwardLayout(l.fcprojOut, l.fchGeluOut, l.fcprojw, l.fcprojb, l.params),
      )
      .addProgram(ResidualProgram.forward(resSizes))(
        _ => resSizes,
        l => ResidualProgram.ResidualForwardLayout(l.residual3Out, l.residual2Out, l.fcprojOut, l.params),
      )

  // ============= Output Pipeline =============

  case class OutputParams(B: Int, T: Int, C: Int, V: Int, Vp: Int)

  case class OutputLayout(
    residualIn: GBuffer[Float32],
    lnfw: GBuffer[Float32],
    lnfb: GBuffer[Float32],
    wte: GBuffer[Float32],
    dummyBias: GBuffer[Float32],  // Unused bias buffer for matmul (hasBias=false)
    lnfOut: GBuffer[Float32],
    lnfMean: GBuffer[Float32],
    lnfRstd: GBuffer[Float32],
    logits: GBuffer[Float32],
    probs: GBuffer[Float32],
    params: GUniform[GPT2Params],
  ) derives Layout

  def buildOutputPipeline(B: Int, T: Int, C: Int, V: Int, Vp: Int): GExecution[OutputParams, OutputLayout, OutputLayout] =
    val lnSizes = LayerNormProgram.LayerNormSizes(B, T, C)
    val mmSizes = MatmulProgram.MatmulSizes(B * T, C, Vp, hasBias = false)
    val smSizes = SoftmaxCrossEntropyProgram.SoftmaxSizes(B, T, V, Vp)

    GExecution[OutputParams, OutputLayout]()
      .addProgram(LayerNormProgram.forward(lnSizes))(
        _ => lnSizes,
        l => LayerNormProgram.LayerNormForwardLayout(l.lnfOut, l.lnfMean, l.lnfRstd, l.residualIn, l.lnfw, l.lnfb, l.params),
      )
      .addProgram(MatmulProgram.forward(mmSizes))(
        _ => mmSizes,
        l => MatmulProgram.MatmulForwardLayout(l.logits, l.lnfOut, l.wte, l.dummyBias, l.params),
      )
      .addProgram(SoftmaxCrossEntropyProgram.softmaxForward(smSizes))(
        _ => smSizes,
        l => SoftmaxCrossEntropyProgram.SoftmaxForwardLayout(l.probs, l.logits, l.params),
      )

  // ============= Full Forward Pass =============

  def forwardGPU(
    model: GPT2Model,
    inputTokens: Array[Int],
    targetTokens: Array[Int],
    B: Int,
    T: Int,
  )(using runtime: CyfraRuntime): (Float, Array[Float], Array[Float]) =
    val config = model.config
    val C = config.channels
    val L = config.numLayers
    val NH = config.numHeads
    val V = config.vocabSize
    val Vp = config.paddedVocabSize
    val maxT = config.maxSeqLen

    // === Step 1: Encoder (GPU) ===
    val encoded = new Array[Float](B * T * C)
    runEncoderGPU(inputTokens, model.wte, model.wpe, encoded, B, T, C, Vp, maxT)

    // === Step 2: Transformer Layers ===
    val residual3 = new Array[Float](L * B * T * C)

    for layer <- 0 until L do
      val residualIn = if layer == 0 then encoded else residual3.slice((layer - 1) * B * T * C, layer * B * T * C)

      // Layer params (sliced)
      val lLn1w = model.ln1w.slice(layer * C, (layer + 1) * C)
      val lLn1b = model.ln1b.slice(layer * C, (layer + 1) * C)
      val lQkvw = model.qkvw.slice(layer * 3 * C * C, (layer + 1) * 3 * C * C)
      val lQkvb = model.qkvb.slice(layer * 3 * C, (layer + 1) * 3 * C)
      val lAttprojw = model.attprojw.slice(layer * C * C, (layer + 1) * C * C)
      val lAttprojb = model.attprojb.slice(layer * C, (layer + 1) * C)
      val lLn2w = model.ln2w.slice(layer * C, (layer + 1) * C)
      val lLn2b = model.ln2b.slice(layer * C, (layer + 1) * C)
      val lFcw = model.fcw.slice(layer * 4 * C * C, (layer + 1) * 4 * C * C)
      val lFcb = model.fcb.slice(layer * 4 * C, (layer + 1) * 4 * C)
      val lFcprojw = model.fcprojw.slice(layer * C * 4 * C, (layer + 1) * C * 4 * C)
      val lFcprojb = model.fcprojb.slice(layer * C, (layer + 1) * C)

      // Layer outputs
      val lLn1Out = new Array[Float](B * T * C)
      val lLn1Mean = new Array[Float](B * T)
      val lLn1Rstd = new Array[Float](B * T)
      val lQkvOut = new Array[Float](B * T * 3 * C)
      val lAttyOut = new Array[Float](B * T * C)
      val lAttprojOut = new Array[Float](B * T * C)
      val lResidual2Out = new Array[Float](B * T * C)
      val lLn2Out = new Array[Float](B * T * C)
      val lLn2Mean = new Array[Float](B * T)
      val lLn2Rstd = new Array[Float](B * T)
      val lFchOut = new Array[Float](B * T * 4 * C)
      val lFchGeluOut = new Array[Float](B * T * 4 * C)
      val lFcprojOut = new Array[Float](B * T * C)
      val lResidual3Out = new Array[Float](B * T * C)

      // Pre-attention pipeline (GPU)
      val preAttPipeline = buildPreAttPipeline(B, T, C)
      val preAttRegion = GBufferRegion
        .allocate[PreAttLayout]
        .map(layout => preAttPipeline.execute(PreAttParams(B, T, C), layout))

      preAttRegion.runUnsafe(
        init = PreAttLayout(
          residualIn = GBuffer(residualIn),
          ln1w = GBuffer(lLn1w), ln1b = GBuffer(lLn1b),
          qkvw = GBuffer(lQkvw), qkvb = GBuffer(lQkvb),
          ln1Out = GBuffer(lLn1Out), ln1Mean = GBuffer(lLn1Mean), ln1Rstd = GBuffer(lLn1Rstd),
          qkvOut = GBuffer(lQkvOut),
          params = GUniform(GPT2Params.forLayerNorm(B, T, C)),  // This creates the actual uniform value
        ),
        onDone = l =>
          l.ln1Out.readArray(lLn1Out)
          l.ln1Mean.readArray(lLn1Mean)
          l.ln1Rstd.readArray(lLn1Rstd)
          l.qkvOut.readArray(lQkvOut),
      )

      // Attention (CPU - complex to port)
      attentionForwardCPU(lAttyOut, lQkvOut, B, T, C, NH)

      // Post-attention pipeline (GPU)
      val postAttPipeline = buildPostAttPipeline(B, T, C)
      val postAttRegion = GBufferRegion
        .allocate[PostAttLayout]
        .map(layout => postAttPipeline.execute(PostAttParams(B, T, C), layout))

      postAttRegion.runUnsafe(
        init = PostAttLayout(
          residualIn = GBuffer(residualIn),
          attyOut = GBuffer(lAttyOut),
          attprojw = GBuffer(lAttprojw), attprojb = GBuffer(lAttprojb),
          ln2w = GBuffer(lLn2w), ln2b = GBuffer(lLn2b),
          fcw = GBuffer(lFcw), fcb = GBuffer(lFcb),
          fcprojw = GBuffer(lFcprojw), fcprojb = GBuffer(lFcprojb),
          attprojOut = GBuffer(lAttprojOut),
          residual2Out = GBuffer(lResidual2Out),
          ln2Out = GBuffer(lLn2Out), ln2Mean = GBuffer(lLn2Mean), ln2Rstd = GBuffer(lLn2Rstd),
          fchOut = GBuffer(lFchOut), fchGeluOut = GBuffer(lFchGeluOut),
          fcprojOut = GBuffer(lFcprojOut), residual3Out = GBuffer(lResidual3Out),
          params = GUniform(GPT2Params.forLayerNorm(B, T, C)),  // This creates the actual uniform value
        ),
        onDone = l =>
          l.residual3Out.readArray(lResidual3Out),
      )

      // Copy to full array
      System.arraycopy(lResidual3Out, 0, residual3, layer * B * T * C, B * T * C)

    // === Step 3: Output stages (GPU) ===
    val finalResidual = residual3.slice((L - 1) * B * T * C, L * B * T * C)
    val lnfOut = new Array[Float](B * T * C)
    val lnfMean = new Array[Float](B * T)
    val lnfRstd = new Array[Float](B * T)
    val logits = new Array[Float](B * T * Vp)
    val probs = new Array[Float](B * T * Vp)

    val outputPipeline = buildOutputPipeline(B, T, C, V, Vp)
    val outputRegion = GBufferRegion
      .allocate[OutputLayout]
      .map(layout => outputPipeline.execute(OutputParams(B, T, C, V, Vp), layout))

    val dummyBias = new Array[Float](Vp)  // Unused bias for matmul

    outputRegion.runUnsafe(
      init = OutputLayout(
        residualIn = GBuffer(finalResidual),
        lnfw = GBuffer(model.lnfw), lnfb = GBuffer(model.lnfb),
        wte = GBuffer(model.wte),
        dummyBias = GBuffer(dummyBias),
        lnfOut = GBuffer(lnfOut), lnfMean = GBuffer(lnfMean), lnfRstd = GBuffer(lnfRstd),
        logits = GBuffer(logits), probs = GBuffer(probs),
        params = GUniform(GPT2Params.forSoftmax(B, T, V, Vp)),
      ),
      onDone = l =>
        l.logits.readArray(logits)
        l.probs.readArray(probs),
    )

    // === Step 4: Cross-entropy (GPU if targets provided) ===
    val meanLoss = if targetTokens != null then
      val losses = new Array[Float](B * T)
      runCrossEntropyGPU(probs, targetTokens, losses, B, T, Vp)
      losses.sum / (B * T)
    else -1.0f

    (meanLoss, logits, probs)

  // ============= Helper Functions =============

  private def runEncoderGPU(
    inp: Array[Int], wte: Array[Float], wpe: Array[Float],
    out: Array[Float], B: Int, T: Int, C: Int, Vp: Int, maxT: Int,
  )(using runtime: CyfraRuntime): Unit =
    val sizes = EncoderProgram.EncoderSizes(B, T, C, Vp, maxT)
    val program = EncoderProgram.forward(sizes)
    val region = GBufferRegion
      .allocate[EncoderProgram.EncoderForwardLayout]
      .map(layout => program.execute(sizes, layout))

    region.runUnsafe(
      init = EncoderProgram.EncoderForwardLayout(
        out = GBuffer(out),
        inp = GBuffer(inp),
        wte = GBuffer(wte),
        wpe = GBuffer(wpe),
        params = GUniform(GPT2Params.forEncoder(B, T, C, maxT)),
      ),
      onDone = _.out.readArray(out),
    )

  private def runCrossEntropyGPU(
    probs: Array[Float], targets: Array[Int], losses: Array[Float],
    B: Int, T: Int, Vp: Int,
  )(using runtime: CyfraRuntime): Unit =
    val sizes = SoftmaxCrossEntropyProgram.CrossEntropySizes(B, T, Vp)
    val program = SoftmaxCrossEntropyProgram.crossEntropyForward(sizes)
    val region = GBufferRegion
      .allocate[SoftmaxCrossEntropyProgram.CrossEntropyForwardLayout]
      .map(layout => program.execute(sizes, layout))

    region.runUnsafe(
      init = SoftmaxCrossEntropyProgram.CrossEntropyForwardLayout(
        losses = GBuffer(losses),
        probs = GBuffer(probs),
        targets = GBuffer(targets),
        params = GUniform(GPT2Params.forCrossEntropy(B, T, Vp)),
      ),
      onDone = _.losses.readArray(losses),
    )

  /** CPU attention (fallback - complex to port) */
  private def attentionForwardCPU(
    out: Array[Float], inp: Array[Float],
    B: Int, T: Int, C: Int, NH: Int,
  ): Unit =
    val C3 = C * 3
    val hs = C / NH
    val scale = 1.0f / math.sqrt(hs).toFloat
    val preatt = new Array[Float](B * NH * T * T)
    val att = new Array[Float](B * NH * T * T)

    for b <- 0 until B; t <- 0 until T; h <- 0 until NH do
      val queryOffset = b * T * C3 + t * C3 + h * hs
      val preattOffset = b * NH * T * T + h * T * T + t * T
      val attOffset = preattOffset

      var maxval = -10000.0f
      for t2 <- 0 to t do
        val keyOffset = b * T * C3 + t2 * C3 + h * hs + C
        var dotprod = 0.0f
        for i <- 0 until hs do dotprod += inp(queryOffset + i) * inp(keyOffset + i)
        dotprod *= scale
        preatt(preattOffset + t2) = dotprod
        if dotprod > maxval then maxval = dotprod

      var expsum = 0.0f
      for t2 <- 0 to t do
        val expv = math.exp(preatt(preattOffset + t2) - maxval).toFloat
        att(attOffset + t2) = expv
        expsum += expv

      val expsumInv = if expsum == 0.0f then 0.0f else 1.0f / expsum
      for t2 <- 0 until T do
        if t2 <= t then att(attOffset + t2) *= expsumInv
        else att(attOffset + t2) = 0.0f

      val outOffset = b * T * C + t * C + h * hs
      for i <- 0 until hs do out(outOffset + i) = 0.0f
      for t2 <- 0 to t do
        val valueOffset = b * T * C3 + t2 * C3 + h * hs + C * 2
        val attWeight = att(attOffset + t2)
        for i <- 0 until hs do out(outOffset + i) += attWeight * inp(valueOffset + i)
