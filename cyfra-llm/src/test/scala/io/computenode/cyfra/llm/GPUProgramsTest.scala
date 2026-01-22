package io.computenode.cyfra.llm

import io.computenode.cyfra.core.{GBufferRegion, GExecution}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.llm.programs.*
import io.computenode.cyfra.llm.programs.EncoderProgram.{EncoderForwardLayout, EncoderSizes}
import io.computenode.cyfra.llm.programs.GeLUProgram.{GeLUForwardLayout, GeLUSizes}
import io.computenode.cyfra.llm.programs.LayerNormProgram.{LayerNormForwardLayout, LayerNormSizes}
import io.computenode.cyfra.llm.programs.MatmulProgram.{MatmulForwardLayout, MatmulSizes}
import io.computenode.cyfra.llm.programs.ResidualProgram.{ResidualForwardLayout, ResidualSizes}
import io.computenode.cyfra.llm.programs.SoftmaxCrossEntropyProgram.{SoftmaxForwardLayout, SoftmaxSizes}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.FunSuite

import scala.math.abs

/** Tests for the actual GPU programs in the programs/ directory.
  *
  * These tests verify that each GPU program produces correct output
  * compared to CPU reference implementations from llm.c.
  */
class GPUProgramsTest extends FunSuite:

  private val tolerance = 1e-4f

  private def maxError(actual: Array[Float], expected: Array[Float]): Float =
    actual.zip(expected).map { case (a, e) => abs(a - e) }.max

  // ==================== CPU Reference Implementations (from llm.c) ====================

  /** gelu_forward from llm.c */
  private def cpuGeluForward(inp: Array[Float]): Array[Float] =
    val scalingFactor = math.sqrt(2.0 / math.Pi).toFloat
    inp.map { x =>
      val cube = 0.044715f * x * x * x
      0.5f * x * (1.0f + math.tanh(scalingFactor * (x + cube)).toFloat)
    }

  /** residual_forward from llm.c */
  private def cpuResidualForward(inp1: Array[Float], inp2: Array[Float]): Array[Float] =
    inp1.zip(inp2).map { case (a, b) => a + b }

  /** layernorm_forward from llm.c */
  private def cpuLayerNormForward(
    inp: Array[Float], weight: Array[Float], bias: Array[Float],
    B: Int, T: Int, C: Int,
  ): (Array[Float], Array[Float], Array[Float]) =
    val eps = 1e-5f
    val out = new Array[Float](B * T * C)
    val mean = new Array[Float](B * T)
    val rstd = new Array[Float](B * T)

    for b <- 0 until B; t <- 0 until T do
      val offset = b * T * C + t * C

      var m = 0.0f
      for i <- 0 until C do m += inp(offset + i)
      m = m / C

      var v = 0.0f
      for i <- 0 until C do
        val xshift = inp(offset + i) - m
        v += xshift * xshift
      v = v / C

      val s = 1.0f / math.sqrt(v + eps).toFloat

      for i <- 0 until C do
        val n = (inp(offset + i) - m) * s
        out(offset + i) = n * weight(i) + bias(i)

      mean(b * T + t) = m
      rstd(b * T + t) = s

    (out, mean, rstd)

  /** matmul_forward from llm.c */
  private def cpuMatmulForward(
    inp: Array[Float], weight: Array[Float], bias: Array[Float],
    BT: Int, C: Int, OC: Int,
  ): Array[Float] =
    val out = new Array[Float](BT * OC)
    for bt <- 0 until BT; o <- 0 until OC do
      var sum = if bias != null then bias(o) else 0.0f
      for i <- 0 until C do sum += inp(bt * C + i) * weight(o * C + i)
      out(bt * OC + o) = sum
    out

  /** softmax_forward from llm.c */
  private def cpuSoftmaxForward(logits: Array[Float], B: Int, T: Int, V: Int, Vp: Int): Array[Float] =
    val probs = new Array[Float](B * T * Vp)
    for b <- 0 until B; t <- 0 until T do
      val offset = b * T * Vp + t * Vp

      var maxval = -10000.0f
      for i <- 0 until V do
        if logits(offset + i) > maxval then maxval = logits(offset + i)

      var sum = 0.0f
      for i <- 0 until V do
        probs(offset + i) = math.exp(logits(offset + i) - maxval).toFloat
        sum += probs(offset + i)

      for i <- 0 until V do probs(offset + i) /= sum
      for i <- V until Vp do probs(offset + i) = 0.0f

    probs

  // ==================== GPU Tests ====================

  test("GeLUProgram.forward matches CPU reference") {
    VkCyfraRuntime.using:
      val N = 1024

      val rng = new scala.util.Random(42)
      val inp = Array.fill(N)(rng.nextFloat() * 4.0f - 2.0f)

      val expectedOut = cpuGeluForward(inp)
      val gpuOut = new Array[Float](N)

      val sizes = GeLUSizes(N)
      val region = GBufferRegion
        .allocate[GeLUForwardLayout]
        .map: layout =>
          GeLUProgram.forward(sizes).execute(sizes, layout)

      region.runUnsafe(
        init = GeLUForwardLayout(
          out = GBuffer(gpuOut),
          inp = GBuffer(inp),
          params = GUniform(GPT2Params.forGelu(N)),
        ),
        onDone = layout => layout.out.readArray(gpuOut),
      )

      val err = maxError(gpuOut, expectedOut)
      println(f"GeLUProgram.forward max error: $err%.6f")
      assert(err < tolerance, s"Max error $err exceeds tolerance $tolerance")
  }

  test("ResidualProgram.forward matches CPU reference") {
    VkCyfraRuntime.using:
      val N = 1024

      val rng = new scala.util.Random(42)
      val inp1 = Array.fill(N)(rng.nextFloat() * 2.0f - 1.0f)
      val inp2 = Array.fill(N)(rng.nextFloat() * 2.0f - 1.0f)

      val expectedOut = cpuResidualForward(inp1, inp2)
      val gpuOut = new Array[Float](N)

      val sizes = ResidualSizes(N)
      val region = GBufferRegion
        .allocate[ResidualForwardLayout]
        .map: layout =>
          ResidualProgram.forward(sizes).execute(sizes, layout)

      region.runUnsafe(
        init = ResidualForwardLayout(
          out = GBuffer(gpuOut),
          inp1 = GBuffer(inp1),
          inp2 = GBuffer(inp2),
          params = GUniform(GPT2Params.forResidual(N)),
        ),
        onDone = layout => layout.out.readArray(gpuOut),
      )

      val err = maxError(gpuOut, expectedOut)
      println(f"ResidualProgram.forward max error: $err%.6f")
      assert(err < tolerance, s"Max error $err exceeds tolerance $tolerance")
  }

  test("EncoderProgram.forward matches CPU reference") {
    VkCyfraRuntime.using:
      val B = 2
      val T = 4
      val C = 8
      val V = 10
      val maxT = T

      val rng = new scala.util.Random(42)
      val inp = Array.fill(B * T)(rng.nextInt(V))
      val wte = Array.fill(V * C)(rng.nextFloat() - 0.5f)
      val wpe = Array.fill(maxT * C)(rng.nextFloat() - 0.5f)

      // CPU reference
      val expectedOut = new Array[Float](B * T * C)
      for b <- 0 until B; t <- 0 until T; i <- 0 until C do
        val ix = inp(b * T + t)
        expectedOut(b * T * C + t * C + i) = wte(ix * C + i) + wpe(t * C + i)

      val gpuOut = new Array[Float](B * T * C)
      val sizes = EncoderSizes(B, T, C, V, maxT)

      val region = GBufferRegion
        .allocate[EncoderForwardLayout]
        .map: layout =>
          EncoderProgram.forward(sizes).execute(sizes, layout)

      region.runUnsafe(
        init = EncoderForwardLayout(
          out = GBuffer(gpuOut),
          inp = GBuffer(inp),
          wte = GBuffer(wte),
          wpe = GBuffer(wpe),
          params = GUniform(GPT2Params.forEncoder(B, T, C, maxT)),
        ),
        onDone = layout => layout.out.readArray(gpuOut),
      )

      val err = maxError(gpuOut, expectedOut)
      println(f"EncoderProgram.forward max error: $err%.6f")
      assert(err < tolerance, s"Max error $err exceeds tolerance $tolerance")
  }

  test("LayerNormProgram.forward matches CPU reference") {
    VkCyfraRuntime.using:
      val B = 2
      val T = 4
      val C = 64

      val rng = new scala.util.Random(42)
      val inp = Array.fill(B * T * C)(rng.nextFloat() * 2.0f - 1.0f)
      val weight = Array.fill(C)(rng.nextFloat() * 0.5f + 0.75f)
      val bias = Array.fill(C)(rng.nextFloat() * 0.2f - 0.1f)

      val (expectedOut, expectedMean, expectedRstd) = cpuLayerNormForward(inp, weight, bias, B, T, C)

      val gpuOut = new Array[Float](B * T * C)
      val gpuMean = new Array[Float](B * T)
      val gpuRstd = new Array[Float](B * T)

      val sizes = LayerNormSizes(B, T, C)

      val region = GBufferRegion
        .allocate[LayerNormForwardLayout]
        .map: layout =>
          LayerNormProgram.forward(sizes).execute(sizes, layout)

      region.runUnsafe(
        init = LayerNormForwardLayout(
          out = GBuffer(gpuOut),
          mean = GBuffer(gpuMean),
          rstd = GBuffer(gpuRstd),
          inp = GBuffer(inp),
          weight = GBuffer(weight),
          bias = GBuffer(bias),
          params = GUniform(GPT2Params.forLayerNorm(B, T, C)),
        ),
        onDone = layout =>
          layout.out.readArray(gpuOut)
          layout.mean.readArray(gpuMean)
          layout.rstd.readArray(gpuRstd),
      )

      val outErr = maxError(gpuOut, expectedOut)
      val meanErr = maxError(gpuMean, expectedMean)
      val rstdErr = maxError(gpuRstd, expectedRstd)

      println(f"LayerNormProgram.forward max errors: out=$outErr%.6f, mean=$meanErr%.6f, rstd=$rstdErr%.6f")
      assert(outErr < 0.001f, s"Output max error $outErr exceeds tolerance")
      assert(meanErr < tolerance, s"Mean max error $meanErr exceeds tolerance")
      assert(rstdErr < tolerance, s"Rstd max error $rstdErr exceeds tolerance")
  }

  test("MatmulProgram.forward matches CPU reference") {
    VkCyfraRuntime.using:
      val BT = 8
      val C = 16
      val OC = 32

      val rng = new scala.util.Random(42)
      val inp = Array.fill(BT * C)(rng.nextFloat() - 0.5f)
      val weight = Array.fill(OC * C)(rng.nextFloat() * 0.1f)
      val bias = Array.fill(OC)(rng.nextFloat() * 0.1f)

      val expectedOut = cpuMatmulForward(inp, weight, bias, BT, C, OC)
      val gpuOut = new Array[Float](BT * OC)

      val sizes = MatmulSizes(BT, C, OC, hasBias = true)

      val region = GBufferRegion
        .allocate[MatmulForwardLayout]
        .map: layout =>
          MatmulProgram.forward(sizes).execute(sizes, layout)

      region.runUnsafe(
        init = MatmulForwardLayout(
          out = GBuffer(gpuOut),
          inp = GBuffer(inp),
          weight = GBuffer(weight),
          bias = GBuffer(bias),
          params = GUniform(GPT2Params.forMatmul(BT, C, OC, true)),
        ),
        onDone = layout => layout.out.readArray(gpuOut),
      )

      val err = maxError(gpuOut, expectedOut)
      println(f"MatmulProgram.forward max error: $err%.6f")
      assert(err < 0.01f, s"Max error $err exceeds tolerance 0.01")
  }

  test("AttentionProgram 5-pass matches CPU reference") {
    VkCyfraRuntime.using:
      val B = 2
      val T = 4
      val C = 16
      val NH = 2
      val hs = C / NH
      val C3 = C * 3

      val rng = new scala.util.Random(42)

      // Input (Q, K, V concatenated)
      val inp = Array.fill(B * T * C3)(rng.nextFloat() * 0.1f)

      // CPU reference - simplified attention
      val cpuOut = new Array[Float](B * T * C)
      val cpuPreatt = new Array[Float](B * NH * T * T)
      val cpuAtt = new Array[Float](B * NH * T * T)
      val scale = 1.0f / math.sqrt(hs).toFloat

      for b <- 0 until B; h <- 0 until NH; t <- 0 until T do
        val queryBase = b * T * C3 + t * C3 + h * hs
        val preattBase = b * NH * T * T + h * T * T + t * T
        val outBase = b * T * C + t * C + h * hs

        // Compute attention scores
        var maxVal = -10000.0f
        for t2 <- 0 until T do
          if t2 <= t then
            val keyBase = b * T * C3 + t2 * C3 + h * hs + C
            var dot = 0.0f
            for i <- 0 until hs do
              dot += inp(queryBase + i) * inp(keyBase + i)
            cpuPreatt(preattBase + t2) = dot * scale
            if cpuPreatt(preattBase + t2) > maxVal then maxVal = cpuPreatt(preattBase + t2)
          else
            cpuPreatt(preattBase + t2) = -10000.0f

        // Softmax
        var expSum = 0.0f
        for t2 <- 0 to t do
          cpuAtt(preattBase + t2) = math.exp(cpuPreatt(preattBase + t2) - maxVal).toFloat
          expSum += cpuAtt(preattBase + t2)
        for t2 <- 0 to t do
          cpuAtt(preattBase + t2) /= expSum
        for t2 <- t + 1 until T do
          cpuAtt(preattBase + t2) = 0.0f

        // Weighted sum of values
        for i <- 0 until hs do cpuOut(outBase + i) = 0.0f
        for t2 <- 0 to t do
          val valueBase = b * T * C3 + t2 * C3 + h * hs + C * 2
          val attWeight = cpuAtt(preattBase + t2)
          for i <- 0 until hs do
            cpuOut(outBase + i) += attWeight * inp(valueBase + i)

      // GPU - 5 passes (preatt, max, expSum, softmax, output)
      val gpuPreatt = new Array[Float](B * NH * T * T)
      val gpuMaxVals = new Array[Float](B * NH * T)
      val gpuExpSums = new Array[Float](B * NH * T)
      val gpuAtt = new Array[Float](B * NH * T * T)
      val gpuOut = new Array[Float](B * T * C)

      val sizes = AttentionProgram.AttentionSizes(B, T, C, NH)
      val gpt2Params = GPT2Params.forAttention(B, T, C, NH)

      // Pass 1: PreAttention (Q @ K^T)
      val preAttProgram = AttentionProgram.preAttention(sizes)
      val preAttRegion = GBufferRegion
        .allocate[AttentionProgram.PreAttLayout]
        .map(layout => preAttProgram.execute(sizes, layout))

      preAttRegion.runUnsafe(
        init = AttentionProgram.PreAttLayout(
          preatt = GBuffer(gpuPreatt),
          inp = GBuffer(inp),
          params = GUniform(gpt2Params),
        ),
        onDone = l => l.preatt.readArray(gpuPreatt),
      )

      val preattErr = cpuPreatt.zip(gpuPreatt).map { case (c, g) => math.abs(c - g) }.max
      println(s"AttentionProgram.preAttention max error: $preattErr")

      // Pass 2: Find max
      val maxProgram = AttentionProgram.attentionMax(sizes)
      val maxRegion = GBufferRegion
        .allocate[AttentionProgram.AttMaxLayout]
        .map(layout => maxProgram.execute(sizes, layout))

      maxRegion.runUnsafe(
        init = AttentionProgram.AttMaxLayout(
          maxVals = GBuffer(gpuMaxVals),
          preatt = GBuffer(gpuPreatt),
          params = GUniform(gpt2Params),
        ),
        onDone = l => l.maxVals.readArray(gpuMaxVals),
      )

      println(s"AttentionProgram.attentionMax completed")

      // Pass 3: Compute exp sum
      val expSumProgram = AttentionProgram.attentionExpSum(sizes)
      val expSumRegion = GBufferRegion
        .allocate[AttentionProgram.AttExpSumLayout]
        .map(layout => expSumProgram.execute(sizes, layout))

      expSumRegion.runUnsafe(
        init = AttentionProgram.AttExpSumLayout(
          expSums = GBuffer(gpuExpSums),
          preatt = GBuffer(gpuPreatt),
          maxVals = GBuffer(gpuMaxVals),
          params = GUniform(gpt2Params),
        ),
        onDone = l => l.expSums.readArray(gpuExpSums),
      )

      println(s"AttentionProgram.attentionExpSum completed")

      // Pass 4: Softmax normalize
      val softmaxProgram = AttentionProgram.attentionSoftmax(sizes)
      val softmaxRegion = GBufferRegion
        .allocate[AttentionProgram.AttSoftmaxLayout]
        .map(layout => softmaxProgram.execute(sizes, layout))

      softmaxRegion.runUnsafe(
        init = AttentionProgram.AttSoftmaxLayout(
          att = GBuffer(gpuAtt),
          preatt = GBuffer(gpuPreatt),
          maxVals = GBuffer(gpuMaxVals),
          expSums = GBuffer(gpuExpSums),
          params = GUniform(gpt2Params),
        ),
        onDone = l => l.att.readArray(gpuAtt),
      )

      val attErr = cpuAtt.zip(gpuAtt).map { case (c, g) => math.abs(c - g) }.max
      println(s"AttentionProgram.attentionSoftmax max error: $attErr")

      // Pass 5: Output (att @ V)
      val outputProgram = AttentionProgram.attentionOutput(sizes)
      val outputRegion = GBufferRegion
        .allocate[AttentionProgram.AttOutputLayout]
        .map(layout => outputProgram.execute(sizes, layout))

      outputRegion.runUnsafe(
        init = AttentionProgram.AttOutputLayout(
          out = GBuffer(gpuOut),
          att = GBuffer(gpuAtt),
          inp = GBuffer(inp),
          params = GUniform(gpt2Params),
        ),
        onDone = l => l.out.readArray(gpuOut),
      )

      val outErr = cpuOut.zip(gpuOut).map { case (c, g) => math.abs(c - g) }.max
      println(s"AttentionProgram.attentionOutput max error: $outErr")

      assert(preattErr < 0.001f, s"PreAtt max error $preattErr exceeds tolerance 0.001")
      assert(attErr < 0.001f, s"Att max error $attErr exceeds tolerance 0.001")
      assert(outErr < 0.001f, s"Output max error $outErr exceeds tolerance 0.001")
  }

  test("SoftmaxCrossEntropyProgram.softmaxForward matches CPU reference") {
    VkCyfraRuntime.using:
      val B = 2
      val T = 4
      val V = 100
      val Vp = 128

      val rng = new scala.util.Random(42)
      val logits = Array.fill(B * T * Vp)(rng.nextFloat() * 4.0f - 2.0f)

      val expectedProbs = cpuSoftmaxForward(logits, B, T, V, Vp)
      val gpuProbs = new Array[Float](B * T * Vp)

      val sizes = SoftmaxSizes(B, T, V, Vp)

      val region = GBufferRegion
        .allocate[SoftmaxForwardLayout]
        .map: layout =>
          SoftmaxCrossEntropyProgram.softmaxForward(sizes).execute(sizes, layout)

      region.runUnsafe(
        init = SoftmaxForwardLayout(
          probs = GBuffer(gpuProbs),
          logits = GBuffer(logits),
          params = GUniform(GPT2Params.forSoftmax(B, T, V, Vp)),
        ),
        onDone = layout => layout.probs.readArray(gpuProbs),
      )

      val err = maxError(gpuProbs, expectedProbs)
      println(f"SoftmaxCrossEntropyProgram.softmaxForward max error: $err%.6f")

      // Verify probabilities sum to ~1 for each position
      for b <- 0 until B; t <- 0 until T do
        val offset = b * T * Vp + t * Vp
        val sum = (0 until V).map(i => gpuProbs(offset + i)).sum
        assert(abs(sum - 1.0f) < 0.01f, s"Probabilities at ($b,$t) sum to $sum, expected ~1.0")

      assert(err < 0.001f, s"Max error $err exceeds tolerance 0.001")
  }
