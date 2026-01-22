package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** Multi-head self-attention split into 5 simpler programs.
  *
  * Splitting avoids JVM/GPU crashes from deeply nested GIO.repeat + GSeq.fold.
  *
  * Pass 1: PreAttention - compute Q @ K^T for each position pair
  * Pass 2: AttentionMax - find max per row for numerical stability
  * Pass 3: AttentionExpSum - compute exp sum for softmax denominator
  * Pass 4: AttentionSoftmax - normalize to get attention weights
  * Pass 5: AttentionOutput - weighted sum of values
  */
object AttentionProgram:

  case class AttentionSizes(B: Int, T: Int, C: Int, NH: Int):
    def totalBHT: Int = B * NH * T
    def totalBHTT: Int = B * NH * T * T
    def hs: Int = C / NH

  // ============= Pass 1: PreAttention (Q @ K^T) =============

  case class PreAttLayout(
    preatt: GBuffer[Float32],  // output: (B, NH, T, T)
    inp: GBuffer[Float32],     // input: Q, K, V (B, T, 3*C)
    params: GUniform[GPT2Params],
  ) derives Layout

  /** Compute scaled dot product attention scores.
    * Each thread computes ONE (b, h, t, t2) dot product.
    */
  def preAttention(sizes: AttentionSizes): GProgram[AttentionSizes, PreAttLayout] =
    GProgram[AttentionSizes, PreAttLayout](
      layout = s =>
        PreAttLayout(
          preatt = GBuffer[Float32](s.totalBHTT),
          inp = GBuffer[Float32](s.B * s.T * 3 * s.C),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.totalBHTT + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val p = layout.params.read
      val B = p.B
      val T = p.T
      val C = p.C
      val NH = p.NH
      val hs = p.OC  // head_size stored in OC
      val C3 = C * 3
      val totalBHTT = B * NH * T * T
      val scale = 1.0f / sqrt(hs.asFloat)

      GIO.when(idx < totalBHTT):
        // Decompose idx into (b, h, t, t2)
        val t2 = idx.mod(T)
        val t = (idx / T).mod(T)
        val h = (idx / (T * T)).mod(NH)
        val b = idx / (NH * T * T)

        // Causal mask: only attend to positions <= t
        val result = when(t2 <= t):
          val queryBase = b * T * C3 + t * C3 + h * hs
          val keyBase = b * T * C3 + t2 * C3 + h * hs + C

          // Simple dot product (each thread does one dot)
          val dotProd = GSeq
            .gen[Int32](0, _ + 1)
            .limit(256)
            .fold(0.0f, (sum: Float32, i: Int32) => {
              when(i < hs):
                val q = GIO.read[Float32](layout.inp, queryBase + i)
                val k = GIO.read[Float32](layout.inp, keyBase + i)
                sum + q * k
              .otherwise(sum)
            })
          dotProd * scale
        .otherwise(-10000.0f)  // Large negative for masked positions

        GIO.write(layout.preatt, idx, result)

  // ============= Pass 2: Find max per (b, h, t) for numerical stability =============

  case class AttMaxLayout(
    maxVals: GBuffer[Float32],  // output: (B, NH, T)
    preatt: GBuffer[Float32],   // input: (B, NH, T, T)
    params: GUniform[GPT2Params],
  ) derives Layout

  /** Find max attention score for each query position */
  def attentionMax(sizes: AttentionSizes): GProgram[AttentionSizes, AttMaxLayout] =
    GProgram[AttentionSizes, AttMaxLayout](
      layout = s =>
        AttMaxLayout(
          maxVals = GBuffer[Float32](s.totalBHT),
          preatt = GBuffer[Float32](s.totalBHTT),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.totalBHT + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val p = layout.params.read
      val B = p.B
      val T = p.T
      val NH = p.NH
      val totalBHT = B * NH * T

      GIO.when(idx < totalBHT):
        val t = idx.mod(T)
        val h = (idx / T).mod(NH)
        val b = idx / (NH * T)
        val attBase = b * NH * T * T + h * T * T + t * T

        val maxVal = GSeq
          .gen[Int32](0, _ + 1)
          .limit(1024)
          .fold(-10000.0f, (curMax: Float32, t2: Int32) => {
            when(t2 <= t):
              val v = GIO.read[Float32](layout.preatt, attBase + t2)
              max(curMax, v)
            .otherwise(curMax)
          })

        GIO.write(layout.maxVals, idx, maxVal)

  // ============= Pass 3: Compute exp sum per (b, h, t) =============

  case class AttExpSumLayout(
    expSums: GBuffer[Float32],  // output: (B, NH, T)
    preatt: GBuffer[Float32],   // input: (B, NH, T, T)
    maxVals: GBuffer[Float32],  // input: (B, NH, T)
    params: GUniform[GPT2Params],
  ) derives Layout

  /** Compute exp sum for softmax denominator */
  def attentionExpSum(sizes: AttentionSizes): GProgram[AttentionSizes, AttExpSumLayout] =
    GProgram[AttentionSizes, AttExpSumLayout](
      layout = s =>
        AttExpSumLayout(
          expSums = GBuffer[Float32](s.totalBHT),
          preatt = GBuffer[Float32](s.totalBHTT),
          maxVals = GBuffer[Float32](s.totalBHT),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.totalBHT + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val p = layout.params.read
      val B = p.B
      val T = p.T
      val NH = p.NH
      val totalBHT = B * NH * T

      GIO.when(idx < totalBHT):
        val t = idx.mod(T)
        val h = (idx / T).mod(NH)
        val b = idx / (NH * T)
        val attBase = b * NH * T * T + h * T * T + t * T
        val maxVal = GIO.read[Float32](layout.maxVals, idx)

        val expSum = GSeq
          .gen[Int32](0, _ + 1)
          .limit(1024)
          .fold(0.0f, (sum: Float32, t2: Int32) => {
            when(t2 <= t):
              val v = GIO.read[Float32](layout.preatt, attBase + t2)
              sum + exp(v - maxVal)
            .otherwise(sum)
          })

        GIO.write(layout.expSums, idx, expSum)

  // ============= Pass 4: Write normalized softmax values =============

  case class AttSoftmaxLayout(
    att: GBuffer[Float32],      // output: (B, NH, T, T)
    preatt: GBuffer[Float32],   // input: (B, NH, T, T)
    maxVals: GBuffer[Float32],  // input: (B, NH, T)
    expSums: GBuffer[Float32],  // input: (B, NH, T)
    params: GUniform[GPT2Params],
  ) derives Layout

  /** Write normalized softmax values - each thread handles ONE output element */
  def attentionSoftmax(sizes: AttentionSizes): GProgram[AttentionSizes, AttSoftmaxLayout] =
    GProgram[AttentionSizes, AttSoftmaxLayout](
      layout = s =>
        AttSoftmaxLayout(
          att = GBuffer[Float32](s.totalBHTT),
          preatt = GBuffer[Float32](s.totalBHTT),
          maxVals = GBuffer[Float32](s.totalBHT),
          expSums = GBuffer[Float32](s.totalBHT),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.totalBHTT + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val p = layout.params.read
      val B = p.B
      val T = p.T
      val NH = p.NH
      val totalBHTT = B * NH * T * T

      GIO.when(idx < totalBHTT):
        // Decompose idx into (b, h, t, t2)
        val t2 = idx.mod(T)
        val t = (idx / T).mod(T)
        val h = (idx / (T * T)).mod(NH)
        val b = idx / (NH * T * T)

        val bhtIdx = b * NH * T + h * T + t
        val maxVal = GIO.read[Float32](layout.maxVals, bhtIdx)
        val expSum = GIO.read[Float32](layout.expSums, bhtIdx)
        val expSumInv = when(expSum === 0.0f)(0.0f).otherwise(1.0f / expSum)

        val v = GIO.read[Float32](layout.preatt, idx)
        val attVal = when(t2 <= t)(exp(v - maxVal) * expSumInv).otherwise(0.0f)
        GIO.write(layout.att, idx, attVal)

  // ============= Pass 5: Output (att @ V) =============

  case class AttOutputLayout(
    out: GBuffer[Float32],     // output: (B, T, C)
    att: GBuffer[Float32],     // input: (B, NH, T, T)
    inp: GBuffer[Float32],     // input: Q, K, V (B, T, 3*C)
    params: GUniform[GPT2Params],
  ) derives Layout

  /** Compute weighted sum of values.
    * Each thread computes ONE output element (b, t, c).
    */
  def attentionOutput(sizes: AttentionSizes): GProgram[AttentionSizes, AttOutputLayout] =
    GProgram[AttentionSizes, AttOutputLayout](
      layout = s =>
        AttOutputLayout(
          out = GBuffer[Float32](s.B * s.T * s.C),
          att = GBuffer[Float32](s.totalBHTT),
          inp = GBuffer[Float32](s.B * s.T * 3 * s.C),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.B * s.T * s.C + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val p = layout.params.read
      val B = p.B
      val T = p.T
      val C = p.C
      val NH = p.NH
      val hs = p.OC  // head_size
      val C3 = C * 3
      val totalOut = B * T * C

      GIO.when(idx < totalOut):
        // Decompose idx into (b, t, c)
        val c = idx.mod(C)
        val t = (idx / C).mod(T)
        val b = idx / (T * C)

        // Find which head this channel belongs to
        val h = c / hs
        val i = c.mod(hs)

        val attBase = b * NH * T * T + h * T * T + t * T

        // Weighted sum over all key positions
        val result = GSeq
          .gen[Int32](0, _ + 1)
          .limit(1024)
          .fold(0.0f, (sum: Float32, t2: Int32) => {
            when(t2 <= t):
              val attWeight = GIO.read[Float32](layout.att, attBase + t2)
              val valueBase = b * T * C3 + t2 * C3 + h * hs + C * 2
              val v = GIO.read[Float32](layout.inp, valueBase + i)
              sum + attWeight * v
            .otherwise(sum)
          })

        GIO.write(layout.out, idx, result)

  // ============= Combined Layout for Pipeline Integration =============

  case class AttentionForwardLayout(
    out: GBuffer[Float32],     // output (B, T, C)
    preatt: GBuffer[Float32],  // intermediate (B, NH, T, T)
    att: GBuffer[Float32],     // intermediate (B, NH, T, T)
    inp: GBuffer[Float32],     // input with Q, K, V (B, T, 3*C)
    params: GUniform[GPT2Params],
  ) derives Layout
