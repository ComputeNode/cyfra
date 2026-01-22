package io.computenode.cyfra.llm

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GExecution, GProgram}
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llm.programs.*

import java.nio.{ByteBuffer, ByteOrder}

/** Optimized GPU Pipeline for GPT-2 forward pass.
  *
  * Key optimizations:
  * - Single GExecution covering ALL operations (no CPU/GPU transfers)
  * - ByteBuffer-based I/O (no Array iteration)
  * - All parameters concatenated; layer offset computed in shader
  * - Attention fully on GPU (5-pass)
  */
object GPT2PipelineOptimized:

  case class PipelineParams(B: Int, T: Int, C: Int, NH: Int, V: Int, Vp: Int, L: Int, maxT: Int)

  /** Master layout with ALL buffers - parameters concatenated, activations reused */
  case class PipelineLayout(
    // Input
    inp: GBuffer[Int32],         // (B, T)
    targets: GBuffer[Int32],     // (B, T)

    // Parameters (concatenated across L layers where applicable)
    wte: GBuffer[Float32],       // (Vp, C)
    wpe: GBuffer[Float32],       // (maxT, C)
    ln1w: GBuffer[Float32],      // (L, C)
    ln1b: GBuffer[Float32],      // (L, C)
    qkvw: GBuffer[Float32],      // (L, 3C, C)
    qkvb: GBuffer[Float32],      // (L, 3C)
    attprojw: GBuffer[Float32],  // (L, C, C)
    attprojb: GBuffer[Float32],  // (L, C)
    ln2w: GBuffer[Float32],      // (L, C)
    ln2b: GBuffer[Float32],      // (L, C)
    fcw: GBuffer[Float32],       // (L, 4C, C)
    fcb: GBuffer[Float32],       // (L, 4C)
    fcprojw: GBuffer[Float32],   // (L, C, 4C)
    fcprojb: GBuffer[Float32],   // (L, C)
    lnfw: GBuffer[Float32],      // (C)
    lnfb: GBuffer[Float32],      // (C)

    // Activations (single-layer sized, reused)
    encoded: GBuffer[Float32],   // (B, T, C)
    residual: GBuffer[Float32],  // (B, T, C) - ping buffer
    residual2: GBuffer[Float32], // (B, T, C) - pong buffer
    ln1Out: GBuffer[Float32],    // (B, T, C)
    ln1Mean: GBuffer[Float32],   // (B, T)
    ln1Rstd: GBuffer[Float32],   // (B, T)
    qkv: GBuffer[Float32],       // (B, T, 3C)
    preatt: GBuffer[Float32],    // (B, NH, T, T)
    attMax: GBuffer[Float32],    // (B, NH, T)
    attExp: GBuffer[Float32],    // (B, NH, T)
    att: GBuffer[Float32],       // (B, NH, T, T)
    atty: GBuffer[Float32],      // (B, T, C)
    attproj: GBuffer[Float32],   // (B, T, C)
    ln2Out: GBuffer[Float32],    // (B, T, C)
    ln2Mean: GBuffer[Float32],   // (B, T)
    ln2Rstd: GBuffer[Float32],   // (B, T)
    fch: GBuffer[Float32],       // (B, T, 4C)
    fchGelu: GBuffer[Float32],   // (B, T, 4C)
    fcproj: GBuffer[Float32],    // (B, T, C)
    lnfOut: GBuffer[Float32],    // (B, T, C)
    lnfMean: GBuffer[Float32],   // (B, T)
    lnfRstd: GBuffer[Float32],   // (B, T)
    logits: GBuffer[Float32],    // (B, T, Vp)
    probs: GBuffer[Float32],     // (B, T, Vp)
    losses: GBuffer[Float32],    // (B, T)
    dummyBias: GBuffer[Float32], // (Vp) for weight-tied logits

    // Params uniform
    params: GUniform[GPT2Params],
  ) derives Layout

  def buildPipeline(p: PipelineParams): GExecution[PipelineParams, PipelineLayout, PipelineLayout] =
    val B = p.B; val T = p.T; val C = p.C; val NH = p.NH
    val V = p.V; val Vp = p.Vp; val L = p.L; val maxT = p.maxT

    // Start with encoder
    val encSizes = EncoderProgram.EncoderSizes(B, T, C, Vp, maxT)
    var pipeline = GExecution[PipelineParams, PipelineLayout]()
      .addProgram(EncoderProgram.forward(encSizes))(
        _ => encSizes,
        l => EncoderProgram.EncoderForwardLayout(l.encoded, l.inp, l.wte, l.wpe, l.params),
      )

    // For each transformer layer
    for layer <- 0 until L do
      val lnSizes = LayerNormProgram.LayerNormSizes(B, T, C, L)
      val qkvSizes = MatmulProgram.MatmulSizes(B * T, C, 3 * C, hasBias = true, L)
      val attSizes = AttentionProgram.AttentionSizes(B, T, C, NH)
      val attprojSizes = MatmulProgram.MatmulSizes(B * T, C, C, hasBias = true, L)
      val resSizes = ResidualProgram.ResidualSizes(B * T * C)
      val ln2Sizes = LayerNormProgram.LayerNormSizes(B, T, C, L)
      val fcSizes = MatmulProgram.MatmulSizes(B * T, C, 4 * C, hasBias = true, L)
      val geluSizes = GeLUProgram.GeLUSizes(B * T * 4 * C)
      val fcprojSizes = MatmulProgram.MatmulSizes(B * T, 4 * C, C, hasBias = true, L)

      // Input for this layer: encoded (layer 0) or residual2 (layers 1+)
      // Output: residual2 (ping-pong between residual and residual2)

      pipeline = pipeline
        // LayerNorm 1 - reads from encoded/residual2, writes to ln1Out
        .addProgram(LayerNormProgram.forward(lnSizes, layer))(  // layer baked in
          _ => lnSizes,
          l => LayerNormProgram.LayerNormForwardLayout(
            l.ln1Out, l.ln1Mean, l.ln1Rstd,
            if layer == 0 then l.encoded else l.residual2,
            l.ln1w, l.ln1b, l.params,
          ),
        )
        // QKV projection
        .addProgram(MatmulProgram.forward(qkvSizes, layer))(  // layer baked in
          _ => qkvSizes,
          l => MatmulProgram.MatmulForwardLayout(l.qkv, l.ln1Out, l.qkvw, l.qkvb, l.params),
        )
        // Attention Pass 1: Q @ K^T
        .addProgram(AttentionProgram.preAttention(attSizes))(
          _ => attSizes,
          l => AttentionProgram.PreAttLayout(l.preatt, l.qkv, l.params),
        )
        // Attention Pass 2: Find max
        .addProgram(AttentionProgram.attentionMax(attSizes))(
          _ => attSizes,
          l => AttentionProgram.AttMaxLayout(l.attMax, l.preatt, l.params),
        )
        // Attention Pass 3: Exp sum
        .addProgram(AttentionProgram.attentionExpSum(attSizes))(
          _ => attSizes,
          l => AttentionProgram.AttExpSumLayout(l.attExp, l.preatt, l.attMax, l.params),
        )
        // Attention Pass 4: Softmax normalize
        .addProgram(AttentionProgram.attentionSoftmax(attSizes))(
          _ => attSizes,
          l => AttentionProgram.AttSoftmaxLayout(l.att, l.preatt, l.attMax, l.attExp, l.params),
        )
        // Attention Pass 5: att @ V
        .addProgram(AttentionProgram.attentionOutput(attSizes))(
          _ => attSizes,
          l => AttentionProgram.AttOutputLayout(l.atty, l.att, l.qkv, l.params),
        )
        // Attention projection
        .addProgram(MatmulProgram.forward(attprojSizes, layer))(  // layer baked in
          _ => attprojSizes,
          l => MatmulProgram.MatmulForwardLayout(l.attproj, l.atty, l.attprojw, l.attprojb, l.params),
        )
        // Residual 1: (encoded/residual2) + attproj -> residual
        .addProgram(ResidualProgram.forward(resSizes))(
          _ => resSizes,
          l => ResidualProgram.ResidualForwardLayout(
            l.residual,
            if layer == 0 then l.encoded else l.residual2,
            l.attproj, l.params,
          ),
        )
        // LayerNorm 2
        .addProgram(LayerNormProgram.forward(ln2Sizes, layer))(  // layer baked in
          _ => ln2Sizes,
          l => LayerNormProgram.LayerNormForwardLayout(l.ln2Out, l.ln2Mean, l.ln2Rstd, l.residual, l.ln2w, l.ln2b, l.params),
        )
        // FC
        .addProgram(MatmulProgram.forward(fcSizes, layer))(  // layer baked in
          _ => fcSizes,
          l => MatmulProgram.MatmulForwardLayout(l.fch, l.ln2Out, l.fcw, l.fcb, l.params),
        )
        // GELU
        .addProgram(GeLUProgram.forward(geluSizes))(
          _ => geluSizes,
          l => GeLUProgram.GeLUForwardLayout(l.fchGelu, l.fch, l.params),
        )
        // FC projection
        .addProgram(MatmulProgram.forward(fcprojSizes, layer))(  // layer baked in
          _ => fcprojSizes,
          l => MatmulProgram.MatmulForwardLayout(l.fcproj, l.fchGelu, l.fcprojw, l.fcprojb, l.params),
        )
        // Residual 2: residual + fcproj -> residual2
        .addProgram(ResidualProgram.forward(resSizes))(
          _ => resSizes,
          l => ResidualProgram.ResidualForwardLayout(l.residual2, l.residual, l.fcproj, l.params),
        )

    // Final layers
    val finalLnSizes = LayerNormProgram.LayerNormSizes(B, T, C, 1)  // L=1 for final LN
    val logitsSizes = MatmulProgram.MatmulSizes(B * T, C, Vp, hasBias = false, 1)
    val softmaxSizes = SoftmaxCrossEntropyProgram.SoftmaxSizes(B, T, V, Vp)
    val ceSizes = SoftmaxCrossEntropyProgram.CrossEntropySizes(B, T, Vp)

    pipeline
      .addProgram(LayerNormProgram.forward(finalLnSizes))(
        _ => finalLnSizes,
        l => LayerNormProgram.LayerNormForwardLayout(l.lnfOut, l.lnfMean, l.lnfRstd, l.residual2, l.lnfw, l.lnfb, l.params),
      )
      .addProgram(MatmulProgram.forward(logitsSizes))(
        _ => logitsSizes,
        l => MatmulProgram.MatmulForwardLayout(l.logits, l.lnfOut, l.wte, l.dummyBias, l.params),
      )
      .addProgram(SoftmaxCrossEntropyProgram.softmaxForward(softmaxSizes))(
        _ => softmaxSizes,
        l => SoftmaxCrossEntropyProgram.SoftmaxForwardLayout(l.probs, l.logits, l.params),
      )
      .addProgram(SoftmaxCrossEntropyProgram.crossEntropyForward(ceSizes))(
        _ => ceSizes,
        l => SoftmaxCrossEntropyProgram.CrossEntropyForwardLayout(l.losses, l.probs, l.targets, l.params),
      )

  // ============= ByteBuffer Utilities =============

  def allocateBuffer(floatCount: Int): ByteBuffer =
    ByteBuffer.allocateDirect(floatCount * 4).order(ByteOrder.nativeOrder())

  def allocateIntBuffer(intCount: Int): ByteBuffer =
    ByteBuffer.allocateDirect(intCount * 4).order(ByteOrder.nativeOrder())

  def copyToBuffer(arr: Array[Float], buf: ByteBuffer): Unit =
    buf.clear(); buf.asFloatBuffer().put(arr); buf.rewind()

  def copyIntToBuffer(arr: Array[Int], buf: ByteBuffer): Unit =
    buf.clear(); buf.asIntBuffer().put(arr); buf.rewind()

  def copyFromBuffer(buf: ByteBuffer, arr: Array[Float]): Unit =
    buf.rewind(); buf.asFloatBuffer().get(arr)

  // ============= Forward Pass =============

  def forwardGPU(
    model: GPT2Model,
    inputBuf: ByteBuffer,
    targetBuf: ByteBuffer,
    B: Int,
    T: Int,
    probsBuf: ByteBuffer,
    lossesBuf: ByteBuffer,
  )(using runtime: CyfraRuntime): Float =
    val config = model.config
    val C = config.channels
    val L = config.numLayers
    val NH = config.numHeads
    val V = config.vocabSize
    val Vp = config.paddedVocabSize
    val maxT = config.maxSeqLen

    val pParams = PipelineParams(B, T, C, NH, V, Vp, L, maxT)
    val pipeline = buildPipeline(pParams)

    // Allocate parameter buffers (can be cached/reused)
    val wteBuf = allocateBuffer(Vp * C); copyToBuffer(model.wte, wteBuf)
    val wpeBuf = allocateBuffer(maxT * C); copyToBuffer(model.wpe, wpeBuf)
    val ln1wBuf = allocateBuffer(L * C); copyToBuffer(model.ln1w, ln1wBuf)
    val ln1bBuf = allocateBuffer(L * C); copyToBuffer(model.ln1b, ln1bBuf)
    val qkvwBuf = allocateBuffer(L * 3 * C * C); copyToBuffer(model.qkvw, qkvwBuf)
    val qkvbBuf = allocateBuffer(L * 3 * C); copyToBuffer(model.qkvb, qkvbBuf)
    val attprojwBuf = allocateBuffer(L * C * C); copyToBuffer(model.attprojw, attprojwBuf)
    val attprojbBuf = allocateBuffer(L * C); copyToBuffer(model.attprojb, attprojbBuf)
    val ln2wBuf = allocateBuffer(L * C); copyToBuffer(model.ln2w, ln2wBuf)
    val ln2bBuf = allocateBuffer(L * C); copyToBuffer(model.ln2b, ln2bBuf)
    val fcwBuf = allocateBuffer(L * 4 * C * C); copyToBuffer(model.fcw, fcwBuf)
    val fcbBuf = allocateBuffer(L * 4 * C); copyToBuffer(model.fcb, fcbBuf)
    val fcprojwBuf = allocateBuffer(L * C * 4 * C); copyToBuffer(model.fcprojw, fcprojwBuf)
    val fcprojbBuf = allocateBuffer(L * C); copyToBuffer(model.fcprojb, fcprojbBuf)
    val lnfwBuf = allocateBuffer(C); copyToBuffer(model.lnfw, lnfwBuf)
    val lnfbBuf = allocateBuffer(C); copyToBuffer(model.lnfb, lnfbBuf)

    // GPU params (layer=0, will be ignored for layer-independent ops)
    val gParams = GPT2Params.forLayer(B, T, C, NH, V, Vp, 0, L, maxT)

    val region = GBufferRegion
      .allocate[PipelineLayout]
      .map(layout => pipeline.execute(pParams, layout))

    var meanLoss = 0.0f
    var runUnsafeStartTime = 0L
    var onDoneStartTime = 0L

    runUnsafeStartTime = System.nanoTime()
    region.runUnsafe(
      init = PipelineLayout(
        inp = GBuffer[Int32](inputBuf),
        targets = GBuffer[Int32](targetBuf),
        wte = GBuffer[Float32](wteBuf),
        wpe = GBuffer[Float32](wpeBuf),
        ln1w = GBuffer[Float32](ln1wBuf),
        ln1b = GBuffer[Float32](ln1bBuf),
        qkvw = GBuffer[Float32](qkvwBuf),
        qkvb = GBuffer[Float32](qkvbBuf),
        attprojw = GBuffer[Float32](attprojwBuf),
        attprojb = GBuffer[Float32](attprojbBuf),
        ln2w = GBuffer[Float32](ln2wBuf),
        ln2b = GBuffer[Float32](ln2bBuf),
        fcw = GBuffer[Float32](fcwBuf),
        fcb = GBuffer[Float32](fcbBuf),
        fcprojw = GBuffer[Float32](fcprojwBuf),
        fcprojb = GBuffer[Float32](fcprojbBuf),
        lnfw = GBuffer[Float32](lnfwBuf),
        lnfb = GBuffer[Float32](lnfbBuf),
        // Activation buffers - just allocate, no data transfer needed
        encoded = GBuffer[Float32](B * T * C),
        residual = GBuffer[Float32](B * T * C),
        residual2 = GBuffer[Float32](B * T * C),
        ln1Out = GBuffer[Float32](B * T * C),
        ln1Mean = GBuffer[Float32](B * T),
        ln1Rstd = GBuffer[Float32](B * T),
        qkv = GBuffer[Float32](B * T * 3 * C),
        preatt = GBuffer[Float32](B * NH * T * T),
        attMax = GBuffer[Float32](B * NH * T),
        attExp = GBuffer[Float32](B * NH * T),
        att = GBuffer[Float32](B * NH * T * T),
        atty = GBuffer[Float32](B * T * C),
        attproj = GBuffer[Float32](B * T * C),
        ln2Out = GBuffer[Float32](B * T * C),
        ln2Mean = GBuffer[Float32](B * T),
        ln2Rstd = GBuffer[Float32](B * T),
        fch = GBuffer[Float32](B * T * 4 * C),
        fchGelu = GBuffer[Float32](B * T * 4 * C),
        fcproj = GBuffer[Float32](B * T * C),
        lnfOut = GBuffer[Float32](B * T * C),
        lnfMean = GBuffer[Float32](B * T),
        lnfRstd = GBuffer[Float32](B * T),
        logits = GBuffer[Float32](B * T * Vp),
        probs = GBuffer[Float32](probsBuf),  // Need ByteBuffer for output
        losses = GBuffer[Float32](lossesBuf), // Need ByteBuffer for output
        dummyBias = GBuffer[Float32](Vp),
        params = GUniform(gParams),
      ),
      onDone = layout =>
        onDoneStartTime = System.nanoTime()
        layout.losses.read(lossesBuf)
        layout.probs.read(probsBuf)
        lossesBuf.rewind()
        val fb = lossesBuf.asFloatBuffer()
        var sum = 0.0f
        for _ <- 0 until B * T do sum += fb.get()
        meanLoss = sum / (B * T),
    )
    val runUnsafeEndTime = System.nanoTime()

    // Timing breakdown:
    // - submitMs: time to allocate buffers, upload data, and submit GPU commands (async)
    // - gpuExecMs: time for GPU kernels to execute (first buffer read blocks until done)
    val totalMs = (runUnsafeEndTime - runUnsafeStartTime) / 1e6
    val submitMs = (onDoneStartTime - runUnsafeStartTime) / 1e6
    val gpuExecMs = (runUnsafeEndTime - onDoneStartTime) / 1e6
    // Note: gpuExecMs includes buffer readback, but that's <1ms, so it's essentially GPU execution
    // println(f"[TIMING] total: $totalMs%.1f ms | submit: $submitMs%.1f ms | GPU exec + readback: $gpuExecMs%.1f ms")

    meanLoss

  def forwardGPUSimple(
    model: GPT2Model,
    inputTokens: Array[Int],
    targetTokens: Array[Int],
    B: Int,
    T: Int,
  )(using runtime: CyfraRuntime): (Float, Array[Float]) =
    val Vp = model.config.paddedVocabSize

    val inputBuf = allocateIntBuffer(B * T)
    val targetBuf = allocateIntBuffer(B * T)
    val probsBuf = allocateBuffer(B * T * Vp)
    val lossesBuf = allocateBuffer(B * T)

    copyIntToBuffer(inputTokens, inputBuf)
    copyIntToBuffer(targetTokens, targetBuf)

    val loss = forwardGPU(model, inputBuf, targetBuf, B, T, probsBuf, lossesBuf)

    val probs = new Array[Float](B * T * Vp)
    copyFromBuffer(probsBuf, probs)

    (loss, probs)
