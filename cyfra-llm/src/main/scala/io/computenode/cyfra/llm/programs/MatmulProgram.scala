package io.computenode.cyfra.llm.programs

import io.computenode.cyfra.core.GProgram
import io.computenode.cyfra.core.GProgram.StaticDispatch
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}

/** Matrix multiplication: out = inp @ weight^T + bias
  *
  * For pipeline use with concatenated parameters (L layers),
  * set L > 1 and layer index in params to read from correct offset.
  */
object MatmulProgram:

  case class MatmulSizes(BT: Int, C: Int, OC: Int, hasBias: Boolean, L: Int = 1):
    def totalOutput: Int = BT * OC

  case class MatmulForwardLayout(
    out: GBuffer[Float32],
    inp: GBuffer[Float32],
    weight: GBuffer[Float32],  // size L*OC*C for pipeline
    bias: GBuffer[Float32],    // size L*OC for pipeline
    params: GUniform[GPT2Params],
  ) derives Layout

  /** Create forward program for a specific layer (layer index baked in) */
  def forward(sizes: MatmulSizes, layerIdx: Int = 0): GProgram[MatmulSizes, MatmulForwardLayout] =
    // Pre-compute offsets as Scala Ints for use as shader constants
    val weightOffsetConst = layerIdx * sizes.OC * sizes.C
    val biasOffsetConst = layerIdx * sizes.OC

    GProgram[MatmulSizes, MatmulForwardLayout](
      layout = s =>
        MatmulForwardLayout(
          out = GBuffer[Float32](s.totalOutput),
          inp = GBuffer[Float32](s.BT * s.C),
          weight = GBuffer[Float32](s.L * s.OC * s.C),
          bias = GBuffer[Float32](s.L * s.OC),
          params = GUniform[GPT2Params](),
        ),
      dispatch = (_, s) => {
        val workgroupSize = 256
        val numWorkgroups = (s.totalOutput + workgroupSize - 1) / workgroupSize
        StaticDispatch((numWorkgroups, 1, 1))
      },
      workgroupSize = (256, 1, 1),
    ): layout =>
      val idx = GIO.invocationId
      val p = layout.params.read
      val BT = p.B  // BT is stored in B field for matmul
      val C = p.C
      val OC = p.OC
      val hasBias = p.hasBias
      val totalOutput = BT * OC

      // Parameter offsets baked in as constants
      val weightOffset: Int32 = weightOffsetConst
      val biasOffset: Int32 = biasOffsetConst

      GIO.when(idx < totalOutput):
        val o = idx.mod(OC)
        val bt = idx / OC

        val biasVal = when(hasBias > 0)(GIO.read[Float32](layout.bias, biasOffset + o)).otherwise(0.0f)

        val dotProduct = GSeq
          .gen[Int32](0, _ + 1)
          .limit(4096)
          .fold(0.0f, (sum: Float32, i: Int32) => {
            when(i < C):
              val inpVal = GIO.read[Float32](layout.inp, bt * C + i)
              val weightVal = GIO.read[Float32](layout.weight, weightOffset + o * C + i)
              sum + inpVal * weightVal
            .otherwise(sum)
          })

        GIO.write(layout.out, idx, dotProduct + biasVal)
